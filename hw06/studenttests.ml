(* Author: Michael Chunko                                                     *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Assert
open X86
open Driver
open Ll
open Backend
open Analysistests
open Datastructures

let exec_ll_ast path ll_ast args extra_files =
  let () = Platform.verb @@ Printf.sprintf "** exec_ll_ast: %s\n" path in

  let output_path = !Platform.output_path in

  (* First - optimize the ll ast *)
  let _ = Opt.do_opt := true in
  let ll_ast = Opt.optimize ll_ast in

  (* Write out the optimized ll file for debugging purposes *)
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let () = write_file dot_ll_file ll_str in

  (* Run the ll backend *)
  let _ = Backend.set_liveness "dataflow" in
  let _ = Backend.set_regalloc "better" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in

  (* Write out the resulting .s file for debugging purposes *)
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let _ = Driver.write_file dot_s_file asm_str in

  (* Create the executable *)
  let exec_file = Platform.gen_name output_path "exec" "" in
  let _ = Platform.link (dot_s_file::extra_files) exec_file in

  (* Run it, piping the output to a temporary file *)
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in
  let result = Driver.run_program args exec_file tmp_file in
  let () = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file) Platform.ignore_error in
  let () = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let exec_ll_file path args =
  let ast = Driver.parse_ll_file path in
  exec_ll_ast path ast args []

let oat_file_e2e_test path args =
  let () = Platform.verb @@ Printf.sprintf "** oat_file_e2e_test: %s\n" path in
  (* Run the Oat typechecker and frontend *)
  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast;
  let ll_ast = Frontend.cmp_prog oat_ast in
  exec_ll_ast path ll_ast args ["runtime.c"]

let pass_all = ref true
let pass_all_executed_ll_file tests =
  List.map (fun (fn, ans) ->
      fn, (fun () ->
          try  assert_eqf (fun () -> exec_ll_file fn "") ans ()
          with exn -> pass_all := false; raise exn))
    tests

let pass_all_executed_oat_file tests =
  List.map (fun (path, args, ans) ->
      (path ^ " args: " ^ args),
      (fun () ->
         try assert_eqf (fun () -> oat_file_e2e_test path args) ans ()
         with exn -> pass_all := false; raise exn))
    tests

let compile_with_config live regalloc ll_ast =
  let open Registers in
  let open Backend in
  let _ = set_liveness live in
  let _ = set_regalloc regalloc in
  let asm_ast = compile_prog ll_ast in
  let (histogram,size) = histogram_of_prog asm_ast in
  histogram, size, asm_ast

let assert_quality fn ll_ast =
  if not !pass_all then failwith "Your register allocator failed a correctness test" else
  let _ = Opt.do_opt := true in
  let ll_ast = Opt.optimize ll_ast in
  let h_greedy, size_greedy, x86_greedy = compile_with_config "dataflow" "greedy" ll_ast in
  let h_better, size_better, x86_better = compile_with_config "dataflow" "better" ll_ast in
  let mem_greedy = Registers.memop_of_prog x86_greedy in
  let mem_better = Registers.memop_of_prog x86_better in
  let _ =
    if !Driver.print_regs_flag then (
      Printf.printf "greedy sz: %4d mem: %4d\t\tbetter sz: %4d mem: %4d \t diff_sz: %4d diff_mem: %4d - %s\n"
      size_greedy mem_greedy size_better mem_better (size_greedy - size_better) (mem_greedy - mem_better) fn) in
  if
    mem_better < mem_greedy then ()
  else if
    size_better < size_greedy then ()
  else failwith @@ Printf.sprintf "greedy is better: size diff = " ^ (string_of_int (size_better - size_greedy))
    ^ ", mem diff = " ^ (string_of_int (mem_better - mem_greedy))

let assert_quality_oat fn () =
  let oat_ast = parse_oat_file fn in
  let ll_ast = Frontend.cmp_prog oat_ast in
  assert_quality fn ll_ast

let quality_oat tests =
  List.map (fun (fn, _, _) -> fn, assert_quality_oat fn) tests


(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test("tetrate correctness", pass_all_executed_oat_file [("regalloc_tetrate.oat", "", "all clear0")]);
  Test("tetrate quality", quality_oat [("regalloc_tetrate.oat", "", "all clear0")]);
]
