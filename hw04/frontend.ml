(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for
     compiling local variable declarations *)

type elt =
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
  let gs, einsns, insns, term_opt, blks = List.fold_left
    (fun (gs, einsns, insns, term_opt, blks) e ->
      match e with
      | L l -> (match term_opt with
        | None ->
          if (List.length insns) = 0 then (gs, einsns, [], None, blks)
          else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                            no terminator" l
        | Some term ->
          (gs, einsns, [], None, (l, {insns; term})::blks)) (* Had to change this too *)
      | T t  -> (gs, einsns, insns, Some (Llutil.Parsing.gensym "tmn", t), blks) (* Had to change this *)
      | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
      | G (gid,gdecl) -> ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
      | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
    ) ([], [], [], None, []) code in
  match term_opt with
  | None -> failwith "build_cfg: entry block has no terminator"
  | Some term -> let insns = einsns @ insns in
    ({insns; term}, blks), gs

(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct
  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None

  let rec print = function
    | (id, (ty, op))::tl -> print_string ("(" ^ id ^ " [" ^ (string_of_ty ty) ^ ", " ^ (string_of_operand op) ^ "])\n"); print tl
    | [] -> ()
end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker. *)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString      -> I8
  | Ast.RArray u     -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) ->
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r

let typ_of_binop : Ast.binop -> Ll.ty * Ll.ty * Ll.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (I64, I64, I64)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (I64, I64, I1)
  | And | Or -> (I1, I1, I1)

let cmp_binop bop ty op1 op2 : insn = let open Ll in
  match bop with
  | Ast.Add  -> Binop (Add, ty, op1, op2)
  | Ast.Mul  -> Binop (Mul, ty, op1, op2)
  | Ast.Sub  -> Binop (Sub, ty, op1, op2)
  | Ast.And  -> Binop (And, ty, op1, op2)
  | Ast.IAnd -> Binop (And, ty, op1, op2)
  | Ast.IOr  -> Binop (Or, ty, op1, op2)
  | Ast.Or   -> Binop (Or, ty, op1, op2)
  | Ast.Shl  -> Binop (Shl, ty, op1, op2)
  | Ast.Shr  -> Binop (Lshr, ty, op1, op2)
  | Ast.Sar  -> Binop (Ashr, ty, op1, op2)
  | Ast.Eq   -> Icmp  (Eq, ty, op1, op2)
  | Ast.Neq  -> Icmp  (Ne, ty, op1, op2)
  | Ast.Lt   -> Icmp  (Slt, ty, op1, op2)
  | Ast.Lte  -> Icmp  (Sle, ty, op1, op2)
  | Ast.Gt   -> Icmp  (Sgt, ty, op1, op2)
  | Ast.Gte  -> Icmp  (Sge, ty, op1, op2)

let typ_of_unop : Ast.unop -> Ll.ty * Ll.ty = function
  | Neg | Bitnot -> (I64, I64)
  | Lognot       -> (I1, I1)

(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack. *)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate an array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = Ptr I64 in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]

let rec type_check (lst1:Ll.ty list) (lst2:Ll.ty list) : unit =
  match lst1, lst2 with
  | x::tl1,y::tl2 ->
    if x <> y then failwith "Invalid types provided"
    else type_check tl1 tl2
  | [],[]         -> ()
  | _             -> failwith "type_check: Type lists are different sizes"

(* Wrapper for cmp_exp to resolve pointers *)
let cmp_op ((ty:Ll.ty), (op:Ll.operand), (s:stream)) : Ll.ty * Ll.operand * stream =
  match ty with
  | Ptr t -> let uid = gensym "ptr_" in
    t, Ll.Id uid, I (uid, Load (ty, op))::s
  | _     -> ty, op, s

(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression.

   Tips:
   - use the provided cmp_ty function!
   - string literals (CStr s) should be hoisted. You'll need to bitcast the
     resulting gid to (Ptr I8)
   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions
   - we found it useful to write a helper function
     cmp_exp_as : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream
     that compiles an expression and optionally inserts a bitcast to the
     desired Ll type. This is useful for dealing with OAT identifiers that
     correspond to gids that don't quite have the type you want *)
let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  let map_cmp_exp (c:Ctxt.t) (exps:Ast.exp node list) : (Ll.ty * Ll.operand) list * stream =
    let rec map_helper (exps:Ast.exp node list) (lst:(Ll.ty * Ll.operand) list) (ss:stream) =
      match exps with
      | e::tl ->
        let ty, op, s = cmp_exp c e in
        map_helper tl (lst @ [ty,op]) (ss @ s)
      | [] -> lst, ss in
    map_helper exps [] [] in
  let map_cmp_op (lst:(Ll.ty * Ll.operand) list) (s:stream) : (Ll.ty * Ll.operand) list * stream =
    let rec map_helper (lst:(Ll.ty * Ll.operand) list) (lst':(Ll.ty * Ll.operand) list) (s:stream) =
      match lst with
      | (ty,op)::tl ->
        let ty', op', s' = cmp_op (ty,op,[]) in
        map_helper tl  (lst' @ [ty',op']) (s >@ s')
      | [] -> lst', s in
    map_helper lst [] s in
  let map_bitcast (lst:(Ll.ty * Ll.operand) list) (s:stream) (tylist:Ll.ty list) : (Ll.ty * Ll.operand) list * stream =
    let rec map_helper (lst:(Ll.ty * Ll.operand) list) (lst':(Ll.ty * Ll.operand) list) (s:stream) (tylist:Ll.ty list) =
      match lst, tylist with
      | (ty1,op1)::tl1, ty2::tl2 -> let newid = gensym "call_bitcast" in
        let s' = I (newid, Bitcast (ty1, op1, ty2)) in
        map_helper tl1 (lst' @ [ty2, Id newid]) (s >:: s') tl2
      | _                        -> lst', s in
    map_helper lst [] s tylist in
  match exp.elt with
  | CNull ty      -> cmp_rty ty, Null, []
  | CBool b       -> (match b with
    | false -> I1, Const 0L, []
    | true  -> I1, Const 1L, [])
  | CInt i        -> I64, Const i, []
  | CStr s        -> let newid = gensym "str_" in
    let len = String.length s + 1 in
    Ptr (Array (len, I8)), Gid newid, [G (newid, (Array (len, I8), GString s))]
  | CArr (ty,es)  -> let newid = gensym "array_" in
    let args', ss' = map_cmp_exp c es in
    let args, ss = map_cmp_op args' ss' in
    let newty = Struct [I64; Array (List.length es, cmp_ty ty)] in
    let rec array_init (args':(Ll.ty * Ll.operand) list) (args:(Ll.ty * Ll.operand) list) (index:int64) =
      match args', args with
      | (ty1',op')::tl', (ty1,op)::tl -> let previd = gensym "array_init_" in
        (match ty1 with
        | Struct [I64; Array (i,x)] ->
          let newty' = Struct [I64; Array (0,x)] in
          let previd' = gensym "array_init_" in
          (*[I (previd''', Load (newty, Id newid))] >@*)
          [I (previd, Gep (Ptr newty, Id newid, [Const 0L; Const 1L; Const index]))] >@
          (*[I (previd', Load (Ptr (Ptr newty'), Id previd))] >@*)
          [I (previd', Bitcast (Ptr ty1, op', Ptr newty'))] >@
          [I (gensym "array_init", Store (Ptr newty', Id previd', Id previd))] >@
          array_init tl' tl (Int64.add index 1L)
        | _ ->
          [I (previd, Gep (Ptr newty, Id newid, [Const 0L; Const 1L; Const index]))] >@
          [I (gensym "array_init", Store (ty1, op, Id previd))] >@
          array_init [] tl (Int64.add index 1L))
      | _,(ty1,op)::tl -> let previd = gensym "array_init_" in
        [I (previd, Gep (Ptr newty, Id newid, [Const 0L; Const 1L; Const index]))] >@
        [I (gensym "array_init", Store (ty1, op, Id previd))] >@
        array_init [] tl (Int64.add index 1L)
      | _ -> [] in
    Ptr newty, Id newid, [I (newid, Alloca (newty))] >@ ss >@ (array_init args' args 0L)
  | NewArr (ty,e) -> let newid = gensym "array_" in
    let newid2 = gensym "array_" in
    let ty1,op1,s1 = cmp_op (cmp_exp c e) in
    let i = match op1 with
      | Const x -> Int64.to_int x
      | _       -> 0 in
    let newty = Struct [I64; Array (i, cmp_ty ty)] in
    let rec array_init  (i:int) (index:int64) =
      if i = 0 then []
      else (let previd = gensym "array_init_" in
        [I (previd, Gep (Ptr newty, Id newid2, [Const 0L; Const 1L; Const index]))] >@
        (match cmp_ty ty with
          | I64 | I8 -> [I (previd, Store (cmp_ty ty, Const 0L, Id previd))]
          | I1       -> [I (previd, Store (cmp_ty ty, Const 0L, Id previd))]
          | _        -> [I (previd, Store (cmp_ty ty, Null, Id previd))]) >@
        array_init (i-1) (Int64.add index 1L)) in
    Ptr newty, Id newid2, s1 >@ [I (newid, Call(Ptr I64, Gid "oat_alloc_array", [I64, op1]))] >@
    [I (newid2, Bitcast(Ptr I64, Id newid, Ptr newty))] >@ (array_init i 0L)
  | Id id         ->
    let (ty, op) = Ctxt.lookup id c in
    ty, op, []
  | Index (src,i) -> let newid = gensym "index_" in
    let ty1, op1, s1 = cmp_exp c src in
    let ty2, op2, s2 = cmp_op (cmp_exp c i) in
    let ty', s' = (match ty1 with
      | Ptr (Ptr (Struct [I64; Array (_,ty')])) ->
        let ty1, op1, s1' = cmp_op (ty1, op1, s1) in
        Ptr ty', s1' >@ [I (newid, Gep (ty1, op1, [Const 0L; Const 1L; op2]))]
      | Ptr (Struct [I64; Array (_,ty')])       ->
        Ptr ty', s1 >@ [I (newid, Gep (ty1, op1, [Const 0L; Const 1L; op2]))]
      | Ptr (Array (_,ty'))                     ->
        Ptr ty', s1 >@ [I (newid, Gep (ty1, op1, [Const 0L; op2]))]
      | _                                       -> failwith "cmp_exp: Index: Invalid type to index") in
    ty', Id newid, s2 >@ s'
  | Call (e,es)   -> let newid = gensym "call_" in
    let ty, op = match e.elt with
      | Id id -> Ctxt.lookup id c
      | _     -> failwith "cmp_exp: call: Invalid function name" in
    let tylst, retty = match ty with
      | Ptr (Fun (tylst, ty')) -> tylst, ty'
      | _                      -> failwith "cmp_exp: call: Not a function" in
    let args, ss = map_cmp_exp c es in
    let args, ss = map_cmp_op args ss in
    let args, ss = map_bitcast args ss tylst in
    retty, Id newid, ss >:: I (newid, Call (retty, op, args))
  | Bop (b,e1,e2) -> let newid = gensym "bop_" in
    let (t1, t2, t3) = typ_of_binop b in
    let (ty1, op1, s1), (ty2, op2, s2) = cmp_op (cmp_exp c e1), cmp_op (cmp_exp c e2) in
    type_check [t1; t2] [ty1; ty2];
    t3, Id newid, s2 >@ s1 >:: I (newid, cmp_binop b t1 op1 op2)
  | Uop (u,e)     -> let newid = gensym "uop_" in
    let ty1, op, s = cmp_op (cmp_exp c e) in
    let (t1, t2) = typ_of_unop u in
    type_check [t1] [ty1];
    t2, Id newid, s >:: match u with
      | Neg    -> I (newid, Binop (Sub, I64, Const 0L, op))
      | Bitnot -> I (newid, Binop (Xor, I64, op, Const (-1L)))
      | Lognot -> I (newid, Icmp (Eq, I1, op, Const 0L))

(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.
   - don't forget to add a bindings to the context for local variable
     declarations
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt
   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement
   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it! *)
let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  let cmp_decl c v =
    let e = snd v in
    let ty, op, s = cmp_exp c e in
    let tynew, opnew, snew = cmp_op (ty, op, s) in
    let is_global = match e.elt with
      | Id s -> (match snd @@ Ctxt.lookup s c with
        | Gid _ -> true
        | _     -> false)
      | _    -> false in
    let uid = gensym ((fst v) ^ "_") in
    let globaluid = gensym "decl_" in
    let ty', s', op' = if is_global
      then tynew, [I (globaluid, Load (ty, op))], Ll.Id globaluid
      else ty, [], op in
    match ty' with
    | Ptr (Struct [I64; Array (i, x)]) -> (match op' with
       | Id op' -> (*print_string ("Array is: " ^ op');*) Ctxt.add c (fst v) (Ptr ty', Id uid), s >:: I (uid, Alloca ty') >::
         I (uid, Store (ty', Id op', Id uid) )
       | _      -> failwith "cmp_decl: Unexpected error in array")
    | Ptr t -> Ctxt.add c (fst v) (Ptr t, Id uid), [I (uid, Alloca t)] >@ snew >:: I (uid, Store (tynew, opnew, Id uid))
    | _     -> Ctxt.add c (fst v) (Ptr ty', Id uid), s >:: I (uid, Alloca ty') >@ s' >::I (uid, Store (ty', op', Id uid)) in
  let cmp_while c (e, lst) =
    let ty, op, s = cmp_exp c e in
    let label = gensym ("lbl") in
      let start_label = label ^ "_start" in
      let loop_label = label ^ "_then" in
      let end_label = label ^ "_end" in
    let block = cmp_block c Void lst in
    let entry_br = [T (Br start_label)] in
    let cbr = [T (Cbr (op, loop_label, end_label))] in
    c, L end_label :: entry_br @ block @ L loop_label :: cbr @ s @ L start_label :: entry_br in
  match stmt.elt with
  | Ret (Some e) ->
    let (ty, op, s) = cmp_op (cmp_exp c e) in
    c, s >:: T (Ret (ty, Some op))
  | Ret None -> c, [T (Ret (Void, None))]
  | Decl v -> cmp_decl c v
  | Assn (n1, n2) ->
    let ty1, op1, s1 = cmp_exp c n1 in
    let id = match op1 with
      | Id id | Gid id -> id
      | _              -> failwith "Assn: Invalid input" in
    let ty2, op2, s2 = cmp_exp c n2 in
    let ss = match ty2, op2 with
      | Ptr (Struct [I64; Array (_,x)]), Id op2' -> let newid = gensym "bitcast_" in
        let newty = (Struct [I64; Array (0,x)]) in print_string ("\nCase 1\n");
        [I (newid, Bitcast (ty2, Id op2', Ptr newty))] >:: I (id, Store(Ptr newty, Id newid, op1))
      | Ptr (Ptr (Struct [I64; Array (i,x)])), Id op2' -> let newid = gensym "bitcast_" in
        let ty2' = Ptr (Struct [I64; Array (i,x)]) in
        let ty1' = match ty1 with
          | Ptr (Ptr x) -> Ptr x
          | _           -> ty1 in
        [ I (newid, Load (ty2, op2))
        ; I (newid ^ "_", Bitcast (ty2', Id newid, ty1'))
        ; I (id, Store(ty1', Id (newid ^ "_"), op1)) ]
      | Ptr _,_ -> let ty2, op2, s2 = cmp_op (ty2, op2, s2) in
        s2 >:: I (id, Store(ty2, op2, op1))
      | _       -> let ty2, op2, s2 = cmp_op (ty2, op2, s2) in
        [I (id, Store(ty2, op2, op1))] in
    c, ss @ s2 @ s1
  | While (e, lst) -> cmp_while c (e,lst)
  | SCall (e, lst) ->
    let ty, op, s = cmp_exp c e in
    let ty', op', s' =  match (List.hd lst).elt with
      | CStr str -> cmp_exp c (List.hd lst)
      | NewArr _ -> failwith "Scall: CArr unimplemented"
      | Id _ -> failwith "SCall: Id unimplemented"
      | Index _ -> failwith "SCall: Index unimplemented"
      | Call _ -> failwith "SCall: Call unimplemented"
      | _ -> failwith "SCall: Unimplemented type passed" in
    let str_id = gensym "print_str_" in
    c, s' >@ [I (str_id, Bitcast (ty', op', Ptr I8))] >@
    [I (gensym "print_string", Call(Void, Gid "print_string", [Ptr I8, Id str_id]))]

  | If (e, then_lst, else_lst) ->
    let (ty, op, s) = cmp_op (cmp_exp c e) in
    let label = gensym "lbl_" in
      let then_label = label ^ "_then" in
      let else_label = label ^ "_else" in
      let exit_label = label ^ "_exit" in
    let then_block = cmp_block c Void then_lst in
    let else_block = cmp_block c Void else_lst in
    if List.length else_block > 0 then
      let cbr = [T (Cbr (op, then_label, else_label))] in
      c, L exit_label :: T (Br exit_label) :: else_block @ L else_label :: T (Br exit_label) :: then_block @ L then_label :: cbr @ s
    else (* Avoids compiling an empty else block if there's no else *)
      let cbr = [T (Cbr (op, then_label, exit_label))] in
      c, L exit_label :: T (Br exit_label) :: then_block @ L then_label :: cbr @ s
  | For (vlst, e, s, slst) ->
    let rec cmp_decl_lst c = function
      | h::tl -> let (c', decl_lst) = cmp_decl c h in
        c', (snd @@ cmp_decl_lst c' tl) @ decl_lst
      | []    -> c, [] in
    let (c', decl_list) = cmp_decl_lst c vlst in
    let cond = match e with
      | Some e' -> e'
      | None    -> { elt = CBool true; loc = "",(0,0),(0,0) } in
    let s' = match s with
      | Some incr -> slst @ [incr]
      | None      -> slst in
    c, (snd @@ cmp_while c' (cond, s')) @ decl_list

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s ->
    let c, stmt_code = cmp_stmt c rt s in
    c, code >@ stmt_code
  ) (c,[]) stmts

(* Adds each function identifer to the context at an
   appropriately translated type.

   NOTE: The Gid of a function is just its source name *)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left (fun c -> function
    | Ast.Gfdecl { elt={ frtyp; fname; args } } ->
      let ft = TRef (RFun (List.map fst args, frtyp)) in
      Ctxt.add c fname (cmp_ty ft, Gid fname)
    | _ -> c
  ) c p

(* Populate a context with bindings for global variables
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C). *)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left (fun c -> function
    | Ast.Gvdecl { elt={ name; init } } ->
      let ty = Ptr (match init.elt with
        | CNull ty      -> (match ty with
          | RArray t -> Ptr (Struct [I64; Array (0, cmp_ty t)])
          | _        -> cmp_rty ty)
        | CBool _       -> I1
        | CInt _        -> I64
        | CStr s        -> Ptr (Array (String.length s + 1, I8))
        | CArr (ty, es) -> Ptr (Struct [I64; Array (List.length es, cmp_ty ty)])
        | NewArr _      -> failwith "Unimplemented global NewArr"
        | _             -> failwith "Unimplemented global type") in
      Ctxt.add c name (ty, Gid name)
    | _ -> c
  ) c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from *)
let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let rec add_args (c:Ctxt.t) (args:(Ast.ty * Ast.id) list) : Ctxt.t =
    match args with
    | (ty,id)::tl -> add_args (Ctxt.add c id (cmp_ty ty, Id id)) tl
    | []          -> c in
  let rec add_params (c:Ctxt.t) (args:(Ast.ty * Ast.id) list) (ss:stream) : Ctxt.t * stream =
    match args with
    | (ty,id)::tl -> let uid = gensym (id ^ "_") in
      let newctxt = Ctxt.add c id (Ptr (cmp_ty ty), Id uid) in
      add_params newctxt tl (ss >@ [I (uid, Alloca (cmp_ty ty))] >@ [I (uid, Store (cmp_ty ty, Id id, Id uid))])
    | []          -> c, ss in
  let elt = f.elt in
  let ret_typ = cmp_ret_ty elt.frtyp in
  let args = elt.args in
  let c = add_args c args in
  let c, param_ss = add_params c args [] in
  let cfg = cfg_of_stream (param_ss >@ cmp_block c ret_typ elt.body) in
  let arg_ids = List.map snd args in
  let arg_tys = (List.map cmp_ty (List.map fst args)) in
  let fun_decl = {f_ty = (arg_tys, cmp_ret_ty elt.frtyp); f_param = arg_ids; f_cfg = fst cfg } in
  (fun_decl, snd cfg)

(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases
   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations *)
let cmp_ginit (e:Ast.exp) : Ll.ginit =
  match e with
  | CInt i  -> GInt i
  | CBool b -> GInt (if b = true then 1L else 0L)
  | _       -> failwith "cmp_ginit unimplemented"

let rec cmp_gexp (c:Ctxt.t) (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  let ty', ginit', lst = match e.elt with
    | CNull t -> (match t with
      | RString         -> failwith "cmp_gexp: Null RString unimplemented"
      | RArray ty       -> let name = gensym "gexp_array_" in
        Ptr (Struct [I64; Array (0, cmp_ty ty)]), GGid name,
        [name, (Struct [I64; Array (0, cmp_ty ty)], GStruct [I64, GInt 0L; Array (0, cmp_ty ty), GArray []])]
      | RFun (tys, rty) -> failwith "cmp_gexp: Null RFun unimplemented")
    | CBool b -> I1, GInt (if b = true then 1L else 0L), []
    | CInt i  -> I64, GInt i, []
    | CStr s  -> Array (String.length s + 1, I8), GString s, []
    | CArr (ty, es) -> let name = gensym "gexp_array_" in
      let length = List.length es in
      let params = (List.map (fun i -> cmp_ty ty, cmp_ginit i.elt) es) in
      Ptr (Struct [I64; Array (length, cmp_ty ty)]), GGid name,
      [name, (Struct [I64; Array (length, cmp_ty ty)], GStruct [I64, GInt (Int64.of_int length); Array (length, cmp_ty ty), GArray params])]
    | Id s -> failwith "cmp_gexp Id"
    | Index (n1, n2) -> failwith "cmp_gexp: Index"
    | _ -> failwith "cmp_gexp: unimplemented type" in
  (ty', ginit'), lst

(* Oat internals function context ------------------------------------------- *)
let internals = [
  "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt =
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls =
    List.fold_right (fun d (fs, gs) ->
      match d with
      | Ast.Gvdecl { elt=gd } ->
        let ll_gd, gs' = cmp_gexp c gd.init in
        (fs, (gd.name, ll_gd)::gs' @ gs)
      | Ast.Gfdecl fd ->
        let fdecl, gs' = cmp_fdecl c fd in
        (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], []) in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }
