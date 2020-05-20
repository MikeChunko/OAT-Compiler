open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j)              -> Int64.compare i j
      | (NonConst, NonConst)
      | (UndefConst, UndefConst)        -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst   -> "NonConst"
      | Const i    -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"
  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t

let cmp_op (u:Ll.operand) (d:fact) : SymConst.t =
   match u with
  | Id u    -> (match Datastructures.UidM.find_opt u d with
    | None   -> SymConst.UndefConst
    | Some s -> s)
  | Const c -> SymConst.Const c
  | Gid g   -> failwith ("Constprop.cmp_op " ^ g)
  | Null    -> SymConst.UndefConst

(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out *)
let insn_flow (u,i:uid * insn) (d:fact) : fact =
  let cmp_bop (op:Ll.bop) (u1:Ll.operand) (u2:Ll.operand) : SymConst.t =
    let op1, op2 = cmp_op u1 d, cmp_op u2 d in
    match op1, op2 with
    | NonConst,_
    | _,NonConst         -> SymConst.NonConst
    | UndefConst,_
    | _,UndefConst       -> SymConst.UndefConst
    | Const c1, Const c2 -> SymConst.Const (match op with
      | Add  -> Int64.add c1 c2
      | Sub  -> Int64.sub c1 c2
      | Mul  -> Int64.mul c1 c2
      | Shl  -> Int64.shift_left c1 (Int64.to_int c2)
      | Lshr -> Int64.shift_right_logical c1 (Int64.to_int c2)
      | Ashr -> Int64.shift_right c1 (Int64.to_int c2)
      | And  -> Int64.logand c1 c2
      | Or   -> Int64.logor c1 c2
      | Xor  -> Int64.logxor c1 c2) in
  let cmp_icmp (op:Ll.cnd) (u1:Ll.operand) (u2:Ll.operand) : SymConst.t =
    let of_bool (b:bool) : int64 =
      if b then 1L else 0L in
    let op1, op2 = cmp_op u1 d, cmp_op u2 d in
    match op1, op2 with
    | NonConst,_
    | _,NonConst         -> SymConst.NonConst
    | UndefConst,_
    | _,UndefConst       -> SymConst.UndefConst
    | Const c1, Const c2 -> SymConst.Const (match op with
      | Eq  -> of_bool (Int64.compare c1 c2 == 0)
      | Ne  -> of_bool (Int64.compare c1 c2 <> 0)
      | Slt -> of_bool (Int64.compare c1 c2 <  0)
      | Sle -> of_bool (Int64.compare c1 c2 <= 0)
      | Sgt -> of_bool (Int64.compare c1 c2 >  0)
      | Sge -> of_bool (Int64.compare c1 c2 >= 0)) in
  match i with
  | Binop(op,_,u1,u2) -> UidM.add u (cmp_bop op u1 u2) d
  | Icmp(op,_,u1,u2)  -> UidM.add u (cmp_icmp op u1 u2) d
  | Store _           -> UidM.add u SymConst.UndefConst d
  | _                 -> UidM.add u SymConst.NonConst d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  =
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let rec combine (ds:fact list) : fact =
      let fact_merge (key:Datastructures.UidM.key) (m1:'a option) (m2:'b option) : 'c option =
        match m2 with
          | None   -> m1
          | Some _ -> m2 in
      match ds with
      | []    -> UidM.empty
      | [h]   -> h
      | h::tl -> UidM.merge fact_merge h (combine tl)
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg

(* run constant propagation on a cfg given analysis results ----------------- *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in

  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    let cmp_op (u:uid) (op:operand) : operand =
      match op with
      | Id u' -> (match Datastructures.UidM.find u' (cb u) with
        | SymConst.Const c -> Const c
        | _                -> op)
      | _    -> op in
    let cmp_term ((u,t):uid * terminator) : Ll.uid * Ll.terminator =
      match t with
      | Ret (ty, Some op)  -> u, Ret (ty, Some (cmp_op u op))
      | Cbr (op,lbl1,lbl2) -> u, Cbr (cmp_op u op, lbl1, lbl2)
      | _                  -> u, t in
    let rec cp_helper (b_old:Ll.block) (b_new:Ll.block) : Ll.block =
      match b_old.insns with
      | []        -> {b_new with insns = List.rev b_new.insns}
      | (u,i)::tl ->
        let newinsn = u, (match i with
          | Binop(bop,ty,op1,op2) -> Binop(bop,ty,cmp_op u op1,cmp_op u op2)
          | Store(ty,op1,op2)     -> Store(ty,cmp_op u op1,op2)
          | Icmp(cnd,ty,op1,op2)  -> Icmp(cnd,ty,cmp_op u op1,cmp_op u op2)
          | Call(ty,op,oplst)     -> Call(ty,op,List.map (fun (ty,op) -> ty,cmp_op u op) oplst)
          | Bitcast(ty1,op,ty2)   -> Bitcast(ty1,cmp_op u op,ty2)
          | Gep(ty,op,oplst)      -> Gep(ty,op,List.map (fun op -> cmp_op u op) oplst)
          | _                     -> i) in
        cp_helper {b_old with insns = tl} {b_new with insns = newinsn::b_new.insns} in
    {cfg with  blocks = Datastructures.LblM.add l (cp_helper b {insns = []; term = cmp_term b.term}) cfg.blocks} in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
