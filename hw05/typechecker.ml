(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l:'a node) err =
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !="

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce additional (possibly mutually recursive)
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword. *)
let rec subtype (c:Tctxt.t) (t1:ty) (t2:ty) : bool =
  match t1, t2 with
  | TBool, TBool
  | TInt, TInt          -> true
  | TNullRef t1', TNullRef t2'
  | TRef t1', TNullRef t2'
  | TRef t1', TRef t2' -> subtype_ref c t1' t2'
  | _                  -> false

(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c:Tctxt.t) (t1:rty) (t2:rty) : bool =
  match t1, t2 with
  | RString, RString               -> true
  | RArray t1', RArray t2'         -> t1' = t2'
  | RStruct t1', RStruct t2'       -> subtype_struct c t1' t2'
  | RFun (ts1,rt1), RFun (ts2,rt2) -> subtype_func c ts1 rt1 ts2 rt2
  | _                              -> false

and subtype_struct (c:Tctxt.t) (id1:id) (id2:id) : bool =
  match lookup_struct_option id2 c with
  | None      -> false
  | Some flst ->
    List.fold_left (fun b x ->
      b && match lookup_field_option id1 x.fieldName c with
      | None    -> false
      | Some ty -> ty = x.ftyp) true flst

and subtype_func (c:Tctxt.t) (ts1:ty list) (rt1:ret_ty) (ts2:ty list) (rt2:ret_ty) : bool =
  if subtype_retty c rt1 rt2
  then subtype_func_params c ts1 ts2
  else false

and subtype_func_params (c:Tctxt.t) (ts1:ty list) (ts2:ty list) : bool =
  match ts1, ts2 with
  | h1::tl1,h2::tl2 -> subtype c h1 h2 && subtype_func_params c tl1 tl2
  | [],[]           -> true
  | _               -> false

and subtype_retty (c:Tctxt.t) (t1:ret_ty) (t2:ret_ty) : bool =
  match t1, t2 with
  | RetVoid, RetVoid       -> true
  | RetVal t1', RetVal t2' -> subtype c t1' t2'
  | _                      -> false

(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules
    - the function should fail using the "type_error" helper function if the
      type is not well formed
    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)
    - tc contains the structure definition context *)
let rec typecheck_ty (l:'a Ast.node) (tc:Tctxt.t) (t:Ast.ty) : unit =
  match t with
  | TInt | TBool          -> ()
  | TRef rt | TNullRef rt -> typecheck_rty l tc rt

and typecheck_rty (l:'a Ast.node) (tc:Tctxt.t) (t:rty) : unit =
  match t with
  | RString         -> ()
  | RStruct id      ->
    (match Tctxt.lookup_struct_option id tc with
    | None   -> type_error l "typecheck_rty: Invalid struct id"
    | Some _ -> ())
  | RArray ty       -> typecheck_ty l tc ty
  | RFun (tys, rty) ->
    List.iter (fun x -> typecheck_ty l tc x) tys;
    typecheck_retty l tc rty

and typecheck_retty (l:'a Ast.node) (tc:Tctxt.t) (t:ret_ty) : unit =
  match t with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty

(* A helper function to determine whether a type allows the null value *)
let is_nullable_ty (t:Ast.ty) : bool =
  match t with
  | TNullRef _ -> true
  | _          -> false

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oat.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.) *)
let rec typecheck_exp (c:Tctxt.t) (e:Ast.exp node) : Ast.ty =
  match e.elt with
  | CNull rty              ->
    typecheck_rty e c rty;
    TNullRef rty
  | CBool _                -> TBool
  | CInt _                 -> TInt
  | CStr _                 -> TRef (RString)
  | Id id                  ->
    (match Tctxt.lookup_option id c with
    | None    -> type_error e ("typecheck_exp: Id: Undefined id " ^ id)
    | Some ty -> ty)
  | CArr (ty,es)           -> failwith "typecheck_exp: CArr"
  | NewArr (ty,e)          -> failwith "typecheck_exp: NewArr"
  | NewArrInit (t,es,id,e) -> failwith "typecheck_exp: NewArrInit"
  | Index (e1,e2)          -> failwith "typecheck_exp: Index"
  | Length e               -> failwith "typecheck_exp: Length"
  | CStruct (id, es)       -> failwith "typecheck_exp: CStruct"
  | Proj (e,id)            -> failwith "typecheck_exp: Prog"
  | Call (e,es)            ->
    let tylist1, retty = match typecheck_exp c e with
      | TRef(RFun(ts,retty)) -> ts,retty
      | _                    -> type_error e "typecheck_exp: Call: Id is not a function" in
    let tylist2 = List.map (typecheck_exp c) es in
    if subtype_func_params c tylist1 tylist2
    then match retty with
      | RetVal ty -> ty
      | RetVoid   -> type_error e "typecheck_exp: Call: Unexpected return type from function"
    else type_error e "typecheck_exp: Call: Invalid parameters passed to function"
  | Bop (binop,e1,e2)      ->
    let t1 = typecheck_exp c e1 in
    let t2 = typecheck_exp c e2 in
    (match binop with
    | Eq
    | Neq ->
      if t1 = t2
      then TBool
      else type_error e "typecheck_exp: Binop: Invalid types for comparison"
    | _   ->
      let ty1,ty2,ty3 = typ_of_binop binop in
      if t1 = ty1 && t2 = ty2
      then ty3
      else type_error e "typecheck_exp: Binop: Incorrect types")
  | Uop (unop,e)           ->
    let ty1,ty2 = typ_of_unop unop in
    if (typecheck_exp c e) = ty1
    then ty2
    else type_error e "typecheck_exp: Uop: Incorrect type"

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement
   This function should implement the statment typechecking rules from oat.pdf.

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement)
     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns

        in the branching statements, the return behavior of the branching
        statement is the conjunction of the return behavior of the two
        branches: both both branches must definitely return in order for
        the whole statement to definitely return.

        Intuitively: if one of the two branches of a conditional does not
        contain a return statement, then the entire conditional statement might
        not return.

        looping constructs never definitely return

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the
     block typecheck rules. *)
let rec typecheck_stmt (tc:Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  match s.elt with
  | Assn (e1,e2)            ->
    let ty1 = typecheck_exp tc e1 in
    (match ty1 with
    | TRef (RFun _) -> type_error s "typecheck_stmt: Assn: Cannot assign to function"
    | _ ->
      if subtype tc (typecheck_exp tc e2) ty1
      then tc, false
      else type_error s "typecheck_stmt: Assn: Invalid types")
  | Decl (id,e)             ->
    let ty = typecheck_exp tc e in
    (match lookup_local_option id tc with
    | None   -> add_local tc id ty, false
    | Some _ -> type_error s ("typecheck_stmt: Decl: Duplicate definition of local id " ^ id))
  | Ret e                   ->
    (match e with
    | None ->
      if to_ret = RetVoid
      then tc, true
      else type_error s "typecheck_stmt: Ret: Expected void return type"
    | Some e ->
      let retty = match to_ret with
        | RetVoid  -> type_error s "typecheck_stmt: Ret: Expected void return type"
        | RetVal t -> t in
      if subtype tc (typecheck_exp tc e) retty
      then tc, true
      else type_error s "typecheck_stmt: Ret: Unexpected return value")
  | SCall (e,es)            -> failwith "typecheck_stmt: SCall"
  | If (e,thn,els)          -> failwith "typecheck_stmt: If"
  | Cast (rty,id,e,thn,els) -> failwith "typecheck_stmt: Cast"
  | For (vlst,e,s,slst)     -> failwith "typecheck_stmt: For"
  | While (e,slst)          -> failwith "typecheck_stmt: While"

let rec typecheck_block (tc:Tctxt.t) (b:Ast.block) (ret:ret_ty) : bool =
  match b with
  | []    -> false
  | [s]   -> snd (typecheck_stmt tc s ret)
  | h::tl ->
    let tc', returns = typecheck_stmt tc h ret in
    if returns
    then type_error h "typecheck_block: Early return"
    else typecheck_block tc' tl ret

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is
   is needed elswhere in the type system. *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | []   -> false
  | h::t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc:Tctxt.t) id fs  (l:'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("typecheck_tdecl: Repeated fields in " ^ id)
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration
    - extends the local context with the types of the formal parameters to the
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns *)
let typecheck_fdecl (tc:Tctxt.t) (f:Ast.fdecl) (l:'a Ast.node) : unit =
  let {frtyp; fname; args; body} = f in
  let functc = List.fold_left (fun c (ty, id) -> add_local c id ty) tc args in
  let returns = typecheck_block functc body frtyp in
  if returns then ()
  else type_error l ("typecheck_fdecl: Does not return")

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields
     H |-s prog ==> H'

   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)
     H ; G1 |-f prog ==> G2

   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context
     H ; G1 |-g prog ==> G2

   NOTE: global initializers may mention function identifiers as
   constants, but can mention only other global values that were declared earlier *)
let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c decl ->
    match decl with
    | Gtdecl tdecl ->
      let id, fields = tdecl.elt in
      (match lookup_struct_option id c with
      | None   ->
        if check_dups fields
        then type_error tdecl ("create_struct_ctxt: " ^ id ^ " has duplicate fields")
        else add_struct c id fields
      | Some _ -> type_error tdecl ("create_struct_ctxt: Duplicate struct id " ^ id))
    | _            -> c
  ) Tctxt.empty p

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  (* Add builtins first *)
  let tc = List.fold_left (fun c (fname, (args, frtyp)) ->
    add_global c fname (TRef (RFun (args, frtyp)))
  ) tc builtins in
  List.fold_left (fun c decl ->
    match decl with
    | Gfdecl fdecl ->
      let {frtyp; fname; args} = fdecl.elt in
      (match lookup_global_option fname c with
      | None   ->
        let args = List.map (fst) args in
        add_global c fname (TRef (RFun (args, frtyp)))
      | Some _ -> type_error fdecl ("create_struct_ctxt: Duplicate function/global id " ^ fname))
    | _            -> c
  ) tc p

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun c decl ->
    match decl with
    | Gvdecl gdecl ->
      let {name; init} = gdecl.elt in
      (match lookup_global_option name c with
      | None   -> add_global c name (typecheck_exp c init)
      | Some _ -> type_error gdecl ("create_struct_ctxt: Duplicate function/global id " ^ name))
    | _            -> c) tc p

(* This function implements the |- prog and the H ; G |- prog
   rules of the oat.pdf specification. *)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l)        -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l
    | _                            -> ()) p
