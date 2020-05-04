(* Author: Michael Chunko                                                     *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
(** Dead Code Elimination  *)
open Ll
open Datastructures

(* expose a top-level analysis operation ------------------------------------ *)
(* TASK: This function should optimize a block by removing dead instructions
   - lb: a function from uids to the live-OUT set at the
     corresponding program point
   - ab: the alias map flowing IN to each program point in the block
   - b: the current ll block

   Note:
     - Call instructions are never considered to be dead (they might produce
       side effects)
     - Store instructions are not dead if the pointer written to is live _or_
       the pointer written to may be aliased.
     - Other instructions are dead if the value they compute is not live.  *)
let dce_block (lb:uid -> Liveness.Fact.t) (ab:uid -> Alias.fact) (b:Ll.block) : Ll.block =
  let rec dce_helper (b_old:Ll.block) (b_new:Ll.block) : Ll.block =
    match b_old.insns with
    | []        -> {b_new with insns = List.rev b_new.insns}
    | (u,i)::tl -> (match i with
      | Call _            -> dce_helper {b_old with insns = tl} {b_new with insns = (u,i)::b_new.insns}
      | Store(_,_,Id u')  ->
        let liveness_cond =  Datastructures.UidS.mem u' (lb u) in
        let alias_cond    = match Datastructures.UidM.find_opt u' (ab u) with
          | Some Alias.SymPtr.MayAlias -> true
          | _                          -> false in
        if liveness_cond || alias_cond
        then dce_helper {b_old with insns = tl} {b_new with insns = (u,i)::b_new.insns}
        else dce_helper {b_old with insns = tl} b_new
      | Store(_,_,Gid u') -> (* Stores to globals should never be optimized out *)
        dce_helper {b_old with insns = tl} {b_new with insns = (u,i)::b_new.insns}
      | _                 ->
        if Datastructures.UidS.mem u (lb u)
        then dce_helper {b_old with insns = tl} {b_new with insns = (u,i)::b_new.insns}
        else dce_helper {b_old with insns = tl} b_new) in
  dce_helper b {b with insns = []}

let run (lg:Liveness.Graph.t) (ag:Alias.Graph.t) (cfg:Cfg.t) : Cfg.t =
  LblS.fold (fun l cfg ->
    let b = Cfg.block cfg l in

    (* compute liveness at each program point for the block *)
    let lb = Liveness.Graph.uid_out lg l in

    (* compute aliases at each program point for the block *)
    let ab = Alias.Graph.uid_in ag l in

    (* compute optimized block *)
    let b' = dce_block lb ab b in
    Cfg.add_block l b' cfg
  ) (Cfg.nodes cfg) cfg
