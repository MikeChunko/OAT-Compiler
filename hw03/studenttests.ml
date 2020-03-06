(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let gep_extra_tests =
[ "custom_llprograms/gep1.ll", 10L
; "custom_llprograms/gep2.ll", 10L
; "custom_llprograms/gep3.ll", 11L ]

let icmp_tests =
[ "custom_llprograms/icmp1.ll", 1L
; "custom_llprograms/icmp2.ll", 0L
; "custom_llprograms/icmp3.ll", 1L ]

let tetrate_tests =
[ "custom_llprograms/power1.ll", 4L
; "custom_llprograms/power2.ll", 1L
; "custom_llprograms/power3.ll", 81L
; "custom_llprograms/tetrate1.ll", 16L
; "custom_llprograms/tetrate2.ll", 1L
; "custom_llprograms/tetrate3.ll", 27L
; "custom_llprograms/tetrate4.ll", 4L
]

let adj_dup_test = ["adj_dups.ll", 1L]

let io_test1 path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::["binary.c"]) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_program args exec_file tmp_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_s_file exec_file tmp_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let executed_io1 tests =
  List.map (fun (fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqf (fun () -> io_test1 fn args) ans)
    tests

let binary_conversion_tests =
	["binary.ll", [], "1";
	 "binary.ll", ["a"], "10";
	 "binary.ll", ["a";"a"], "11";
	 "binary.ll", ["a";"a";"a"], "100";
	 "binary.ll", ["a";"a";"a";"a"], "101";
	]

let io_test path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::["zpinterop.c"]) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_program args exec_file tmp_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_s_file exec_file tmp_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let executed_io tests =
  List.map (fun (fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqfs (fun () -> io_test fn args) ans)
    tests

let poly1 = ["-2"; "0"; "4"; "-1"; "10"]
let poly2 = ["1"; "0"; "514"; "0"; "0"; "-3920"; "0"; "0"]

let zp_tests =
[ "zptests/zptest.ll", ["100"; "5"], "5"
; "zptests/zptest.ll", ["5"; "0"; "1"], "1"
; "zptests/zptest.ll", ["5"; "1"; "0"], "5"
; "zptests/zptest.ll", "0"::poly1, "10"
; "zptests/zptest.ll", "5"::poly1, "-1145"
; "zptests/zptest.ll", "-3"::poly1, "-113"
; "zptests/zptest.ll", "0"::poly2, "0"
; "zptests/zptest.ll", "14"::poly2, "381086720"
; "zptests/zptest.ll", "-6"::poly2, "-4417920"
]

let russian_peasant_mult_tests =
	["russianMult.ll", 7566L]

let trig_tests = [
  ("./trig_ll/test1.ll", 1L);
  ("./trig_ll/test2.ll", 3L);
  ("./trig_ll/test3.ll", 6L);
  ("./trig_ll/test10.ll", 55L);
  ("./trig_ll/test20.ll", 210L);
]

let provided_tests : suite = [
  GradedTest ("icmp tests", 0, executed icmp_tests);
  GradedTest ("gep_extra_tests", 0, executed gep_extra_tests);
  GradedTest ("tetrate_tests", 0, executed tetrate_tests);
  GradedTest("adj_dup student test", 1, executed adj_dup_test);
  Test("binary conversion tests", executed_io1 binary_conversion_tests);
  Test("Euler02 Lite", Gradedtests.executed ["euler02.ll", 188L]);
  Test("ZP IO Tests", executed_io zp_tests);
  Test("Linked list cycle test", Gradedtests.executed ["llprograms/linkedlist_cycle.ll", 6L]);
  Test("non-duplicate test", Gradedtests.executed ["onenondup.ll", 5L]);
  Test("quicksort test", Gradedtests.executed ["quicksort.ll", 140L]);
  GradedTest("russian peasant tests", 5, Gradedtests.executed russian_peasant_mult_tests);
  GradedTest("triangle_numbers", 5, Gradedtests.executed trig_tests)
]
