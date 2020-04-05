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

let unique_char_tests = [
  ("hw4programs/uniqueChars.oat", "", "0");
  ("hw4programs/uniqueChars1.oat", "", "1");
  ("hw4programs/uniqueChars2.oat", "", "0");
]

let convert (x,y : int * int list) =
  "inversion_counter.oat",
  String.concat " " @@ List.map string_of_int y,
  string_of_int x

(* Each list element is a pair of the correct number of inversions, and the list of integers *)
let inversion_counter_tests =
  List.map convert
  [ 0,   [0]
  ; 0,   [1]
  ; 0,   [-1]
  ; 0,   [159]
  ; 0,   [-6720]
  ; 0,   [1;2]
  ; 1,   [5;3]
  ; 1,   [1;-1]
  ; 0,   [1;2;3]
  ; 1,   [1;3;2]
  ; 1,   [3;2;4]
  ; 3,   [32;29;11]
  ; 0,   [5;5;5]
  ; 2,   [5;5;-43]
  ; 1,   [-4922;-4095;5772;-2762]
  ; 5,   [-646;883;-606;-972;713]
  ; 7,   [6485;302;-6354;4910;-1845]
  ; 20,  [2752;-5496;1743;-1261;9572;7307;2883;4759;-3965;2765]
  ; 65,  [27;-86;17;-12;-75;24;19;-60;-32;-2;-3;52;-33;27;43;-4;36;-1;37;28]
  ; 237, [-81730;-87355;99291;5604;39179;-30213;30890;-43983;-4079;-90803;-35131;-33536;-57490;10889;-42921;44280;-57396;-96971;61723;-64373;48953;-44250;-40866;81793;-32845;27789;-32214;-81704;-97264;-60539]
  ]

let brainfuck_tests = [ ("brainfuck.oat", "+++[r++l-]r.", "660") ]

let coin_row_tests = [
  ("coin_row.oat", "", "17")
]

let quicksort_test = [
   ("quicksort.oat", "", string_of_int (123456789 mod 256));
]

let student_test = [
  ("trianglenumbers.oat", "", "21")
]

let unique_char_tests1 = [
  ("hw4programs/tim_sort.oat", "", "571921230");
]

let array_dups = [
  ("arrayduplicate.oat", "", "4")
]

let my_tests = [
  ("removeAdjDuplicates.oat", "", "15")
]

let studenttests = [
    ("quicksort1.oat", "a c e d b", "a b c d e");
    ("countingsort1.oat", "aaaassssddddffff", "aaaaddddffffssss")
  ]

let lis_tests = [
  ("hw4programs/longest_increasing_subsequence.oat", "", "Usage: ./executable <num 0> <num 1> ...1");
  ("hw4programs/longest_increasing_subsequence.oat", "1 2 3 4 5", "For input [1, 2, 3, 4, 5] the longest increasing subsequence was: [1, 2, 3, 4, 5] with length 50");
  ("hw4programs/longest_increasing_subsequence.oat", "10 9 -2 5 3 7 101 18", "For input [10, 9, -2, 5, 3, 7, 101, 18] the longest increasing subsequence was: [-2, 5, 7, 101] with length 40");
  ("hw4programs/longest_increasing_subsequence.oat", "-100 2 78 3 4 5 -1 5 9", "For input [-100, 2, 78, 3, 4, 5, -1, 5, 9] the longest increasing subsequence was: [-100, 2, 3, 4, 5, 9] with length 60");
]

let determinant_test = [
    ("determinant_size.oat", "94", "")
  ]


let provided_tests : suite = [
  GradedTest("tetrate tests", 0, executed_oat_file tetrate_tests);
  GradedTest("Unique Char tests", 1, executed_oat_file (unique_char_tests));
  Test("jp-oz inversion counter", Gradedtests.executed_oat_file inversion_counter_tests);
  GradedTest("brainfuck test", 10, executed_oat_file brainfuck_tests);
  GradedTest("coin_row tests", 0, executed_oat_file coin_row_tests);
  GradedTest("squicksort tests", 0 , executed_oat_file quicksort_test);
  Test("triangle numbers", Gradedtests.executed_oat_file student_test);
  GradedTest("tim sort Tests", 1, executed_oat_file (unique_char_tests1));
  Test("array duplicates", Gradedtests.executed_oat_file array_dups);
  Test("Semiprime Test",
  (executed_oat_file [("semiprime.oat", "", "4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 0")]));
  GradedTest(
    "Student Tests",
    0,
    executed_oat_file my_tests
  );
  GradedTest("student tests", 5, executed_oat_file studenttests);
  Test("maximum of three numbers test", executed_oat_file ["maxthree.oat", "", "26"]);
  Test("longest increasing subsequence tests", Gradedtests.executed_oat_file lis_tests);
  GradedTest("student tests", 0 , executed_oat_file determinant_test);
]
