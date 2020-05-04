(* Author: Michael Chunko                                                     *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Datastructures

(* dataflow analysis graph signature ---------------------------------------- *)
(* Interface for dataflow graphs structured in a way to facilitate
   the general iterative dataflow analysis algorithm.

   The AsGraph functor in cfg.ml provides an implementation of this
   DFA_GRAPH signature that converts an LL IR control-flow graph to
   this representation.

   NOTE: The direction of the analysis is goverened by how preds and
   succs are instantiated and how the corresponding flow function
   is defined.  This module pretends that all information is flowing
   "forward", but e.g. liveness instantiates the graph so that "forward"
   here is "backward" in the control-flow graph.

   This means that for a node n, the output information is explicitly
   represented by the "find_fact" function:
     out[n] = find_fact g n
   The input information for [n] is implicitly represented by:
     in[n] = combine preds[n] (out[n]) *)
module type DFA_GRAPH =
  sig
    module NodeS : SetS

    (* type of nodes in this graph *)
    type node = NodeS.elt

    (* dataflow facts associated with the out-edges of the nodes in the graph *)
    type fact

    (* the abstract type of dataflow graphs *)
    type t
    val preds : t -> node -> NodeS.t
    val succs : t -> node -> NodeS.t
    val nodes : t -> NodeS.t

    (* the flow function:
       given a graph node and input fact, compute the resulting fact on the
       output edge of the node *)
    val flow : t -> node -> fact -> fact

    (* lookup / modify the dataflow annotations associated with a node *)
    val out : t -> node -> fact
    val add_fact : node -> fact -> t -> t

    (* printing *)
    val to_string : t -> string
    val printer : Format.formatter -> t -> unit
  end

(* abstract dataflow lattice signature -------------------------------------- *)
(* The general algorithm works over a generic lattice of abstract "facts".
    - facts can be combined (this is the 'meet' operation)
    - facts can be compared                                                   *)
module type FACT =
  sig
    type t
    val combine : t list -> t
    val compare : t -> t -> int
    val to_string : t -> string
  end

(* generic iterative dataflow solver ---------------------------------------- *)
(* This functor takes two modules:
      Fact  - the implementation of the lattice
      Graph - the dataflow anlaysis graph

   It produces a module that has a single function 'solve', which
   implements the iterative dataflow analysis described in lecture.
      - using a worklist (or workset) nodes
        [initialized with the set of all nodes]

      - process the worklist until empty:
          . choose a node from the worklist
          . find the node's predecessors and combine their flow facts
          . apply the flow function to the combined input to find the new
            output
          . if the output has changed, update the graph and add the node's
            successors to the worklist *)
module Make (Fact:FACT) (Graph:DFA_GRAPH with type fact := Fact.t) =
  struct
    let solve (g:Graph.t) : Graph.t =
      let rec solve_helper (g:Graph.t) (nodeset:Graph.NodeS.t) : Graph.t =
        if Graph.NodeS.is_empty nodeset
        then g
        else
          let node = Graph.NodeS.choose nodeset in
          let preds = Graph.NodeS.elements (Graph.preds g node) in
          let pred_facts_combined = Fact.combine (
            List.map (fun pred -> Graph.out g pred) preds) in
          let new_output = Graph.flow g node pred_facts_combined in
          let nodeset' = Graph.NodeS.remove node nodeset in
          if Fact.compare new_output (Graph.out g node) <> 0
          then
            let nodeset' = Graph.NodeS.union nodeset' (Graph.succs g node) in
            solve_helper (Graph.add_fact node new_output g) nodeset'
          else
            solve_helper g nodeset' in
      solve_helper g (Graph.nodes g)
  end
