open Ast
open Astlib
open Assert
open Driver

let assert_eq_ast (f: 'a -> 'a -> bool) (x: 'a) (y: unit -> 'a) : assertion =
  fun () -> if f x (y ())  then () else failwith "not equal"

let oat_file_test path args =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in
  
  let output_path = !Platform.output_path in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in  

  let oat_ast = parse_oat_file path in
  let ll_ast = Frontend.cmp_prog oat_ast in
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let () = write_file dot_ll_file ll_str in
  let () = Platform.link (dot_ll_file::["runtime.c"]) exec_file in

  let result = Driver.run_program_error args exec_file tmp_file in
  (*  let () = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file) Platform.ignore_error in *)
  let () = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let executed_oat_file tests =
  List.map (fun (path, args, ans) ->
      (path ^ " args: " ^ args), assert_eqf (fun () -> oat_file_test path args) ans)
    tests

let tetrate_tests = [
  ("tetrate.oat", "", "all clear0")
]

let provided_tests : suite = [
  GradedTest("tetrate tests", 0, executed_oat_file tetrate_tests);
]
