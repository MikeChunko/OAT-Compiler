(* Alias Analysis *)
open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Pervasives.compare

    let to_string = function
      | MayAlias   -> "MayAlias"
      | Unique     -> "Unique"
      | UndefAlias -> "UndefAlias"
  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     (as the value being stored) may be aliased
   - Other instructions do not define pointers *)
let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  let rec call_add_args (args:(ty * operand) list) (d:fact) : fact =
    match args with
    | []          -> d
    | (ty,op)::tl -> (match ty,op with
      | Ptr _, Id u -> UidM.add u SymPtr.MayAlias d
      | _           -> call_add_args tl d) in
  match i with
  | Alloca _              -> UidM.add u SymPtr.Unique d
  | Load (Ptr (Ptr _), _) -> UidM.add u SymPtr.MayAlias d
  | Call (Ptr _, _, args) -> UidM.add u SymPtr.MayAlias (call_add_args args d)
  | Bitcast (Ptr _, u', _)
  | Gep (_, u', _)        -> (match u' with
    | Ll.Id u' -> UidM.add u' SymPtr.MayAlias (UidM.add u SymPtr.MayAlias d)
    | _        -> UidM.add u  SymPtr.MayAlias d)
  | Store (Ptr _,Id u',_) -> UidM.add u' SymPtr.MayAlias d
  | _                     -> UidM.add u  SymPtr.UndefAlias d

(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int =
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* The alias analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful. *)
    let rec combine (ds:fact list) : fact =
      let fact_merge (key:Datastructures.UidM.key) (m1:'a option) (m2:'b option) : 'c option =
        match m1 with
          | None     -> m2
          | Some lvl -> (match lvl with
            | SymPtr.Unique     -> m1
            | SymPtr.MayAlias
            | SymPtr.UndefAlias -> (match m2 with
              | Some SymPtr.Unique
              | Some SymPtr.MayAlias -> m2
              | _                    -> m1)) in
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
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter
     to the function may be aliased *)
  let alias_in =
    List.fold_right
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m)
      g.Cfg.args UidM.empty in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg
