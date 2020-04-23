open Ast
open Assert
open X86
open Driver
open Ll
open Backend

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let fdecl_of_path path =
  Platform.verb @@ Printf.sprintf "* processing file: %s\n" path;
  let ll_ast = parse_ll_file path in
  match ll_ast.Ll.fdecls with
  | [_, fdecl] -> fdecl
  | _ -> failwith "test expected one fdecl"


let exec_e2e_ast ll_ast args extra_files =
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::extra_files) exec_file in
  let result = Driver.run_executable args exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable exited with: %d\n" result in
  Int64.of_int result


let exec_e2e_file path args =
  let ast = Driver.parse_ll_file path in
  exec_e2e_ast ast args []

let io_test path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::["cinterop.c"]) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_program args exec_file tmp_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_s_file exec_file tmp_file) Platform.ignore_error in
  let _ = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let c_link_test c_files path args =
  let ll_ast = Driver.parse_ll_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::c_files) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_executable args exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
    Int64.of_int result

let oat_file_test path args =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in

  let output_path = !Platform.output_path in
  let dot_ll_file = Platform.gen_name output_path "test" ".ll" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let tmp_file = Platform.gen_name output_path "tmp" ".txt" in

  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast;
  let ll_ast = Frontend.cmp_prog oat_ast in
  let ll_str = Driver.string_of_ll_ast path ll_ast in
  let () = write_file dot_ll_file ll_str in
  let () = Platform.link (dot_ll_file::["runtime.c"]) exec_file in

  let result = Driver.run_program args exec_file tmp_file in
  let () = Platform.sh (Printf.sprintf "rm -f %s %s %s" dot_ll_file exec_file tmp_file) Platform.ignore_error in
  let () = Platform.verb @@ Printf.sprintf "** Executable output:\n%s\n" result in
  result

let oat_tc_ok_file_test path =
  let _ = Platform.verb @@ Printf.sprintf "** OAT Typechecker OK Processing: %s\n" path in
  let oat_ast = parse_oat_file path in
  let _= Typechecker.typecheck_program oat_ast in
  ()

let oat_tc_err_file_test path err =
  let _ = Platform.verb @@ Printf.sprintf "** OAT Typechecker Error Processing: %s\n" path in
  let oat_ast = parse_oat_file path in
  try
    let _ = Typechecker.typecheck_program oat_ast in
    failwith @@ Printf.sprintf "Expected type error: %s" err
  with
  | Typechecker.TypeError s ->
    if s = err then () else
      failwith @@ Printf.sprintf "Expected type error: %s but got %s" err s

let executed tests =
  List.map (fun (fn, ans) ->
      fn, assert_eqf (fun () -> exec_e2e_file fn "") ans)
    tests

let executed_oat_file tests =
  List.map (fun (path, args, ans) ->
      (path ^ " args: " ^ args), assert_eqfs (fun () -> oat_file_test path args) ans)
    tests

let executed_tc_ok_file tests =
  List.map (fun path ->
      ("typechecking: " ^ path, fun () -> oat_tc_ok_file_test path)) tests

let executed_tc_err_file tests =
  List.map (fun (path, err) ->
      ("typechecking: " ^ path, fun () -> oat_tc_err_file_test path err)) tests

let executed_io tests =
  List.map (fun (fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqfs (fun () -> io_test fn args) ans)
    tests

let executed_c_link tests =
  List.map (fun (c_file, fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqf (fun () -> c_link_test c_file fn args) ans)
    tests

let typecheck path () =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in
  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast

let typecheck_error (a : assertion) () =
  try a (); failwith "Should have a type error" with Typechecker.TypeError s -> ()

let typecheck_correct (a : assertion) () =
  try a () with Typechecker.TypeError s -> failwith "Should not have had a type error"


let typecheck_file_error tests =
  List.map (fun p -> p, typecheck_error (typecheck p)) tests

let typecheck_file_correct tests =
  List.map (fun p -> p, typecheck_correct (typecheck p)) tests

let unit_tests = [
  "subtype_stringQ_stringQ",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TRef RString; TBool], RetVoid))) (TNullRef (RFun ([TRef RString; TBool], RetVoid))) then ()
       else failwith "should not fail")
; ("no_subtype_stringQ_stringQ",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TBool; TRef RString], RetVoid))) (TRef (RFun ([TRef RString; TBool], RetVoid))) then
         failwith "should not succeed" else ())
  )
]

let tetrate_tests = [
  ("tetrate.oat", "", "all clear0")
]

let typecheck_unit_tests = [
  "subtype_intarray_intarray",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt)) then ()
       else failwith "should not fail")
; ("no_subtype_intarray_boolarray",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TBool)) then
         failwith "should not succeed" else ())
  )
]

let struct_context : (Ast.id * (Ast.id * Ast.ty) list) list -> Tctxt.t =
  List.fold_left (fun tc (sname, fields) ->
      let field_spec = List.map (fun (f, t) -> Ast.{fieldName=f; ftyp=t}) fields in
      Tctxt.add_struct tc sname field_spec) Tctxt.empty

let (=>) arg_types return_type =
  Ast.(TRef (RFun (arg_types, match return_type with Some t -> RetVal t | None -> RetVoid)))

let subtype_tests1 = [
    "subtype_t[]_t[]?",
    (fun () ->
      if not @@ Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TNullRef (RArray TInt))
      then failwith "int[] !<: int[]?"
    );
    "subtype_structA_structB",
    (fun () ->
      let tc = struct_context ["A", []; "B", ["x", TInt]] in
      if not @@ Typechecker.subtype tc (TRef (RStruct "B")) (TRef (RStruct "B"))
      then failwith "B !<: A"
    );
    "subtype_C_B",
    (fun () ->
      let tc = struct_context ["A", []; "B", ["x", TInt]; "C", ["x", TInt; "y", TBool]] in
      if not @@ Typechecker.subtype tc (TRef (RStruct "C")) (TRef (RStruct "B"))
      then failwith "C !<: B");
    "subtype_C->B_B->B",
    (fun () ->
      let tc = struct_context ["A", [];
                               "B", ["x", TInt; "y", TInt];
                               "C", ["x", TInt; "y", TInt; "z", TInt]] in
      let c = Ast.TRef (RStruct "C") in
      let b = Ast.TRef (RStruct "B") in
      if not @@ Typechecker.subtype tc ([b] => Some b) ([c] => Some b)
      then failwith "C !<: B");
    "subtype_C->B_B->B",
    (fun () ->
      let tc = struct_context ["A", []; "B", ["x", TInt]; "C", ["x", TInt; "y", TBool]] in
      let c = Ast.TRef (RStruct "C") in
      let b = Ast.TRef (RStruct "B") in
      if Typechecker.subtype tc ([c] => Some b) ([b] => Some b)
      then failwith "C <: B should not imply ");
    "subtype_S_T",
    (fun () ->
      let tc = struct_context ["S", []; "T", []] in
      let s, t = Ast.(TRef (RStruct "S"), TRef (RStruct "T")) in
      if not @@ Typechecker.subtype tc s t
      then failwith "S !<: T"
    );
    "subtype_T_S",
    (fun () ->
      let tc = struct_context ["S", []; "T", []] in
      let s, t = Ast.(TRef (RStruct "S"), TRef (RStruct "T")) in
      if not @@ Typechecker.subtype tc t s
      then failwith "T !<: S"
    )
  ]

let studenttests1 = [
    (
      "trie.oat",
      "this is a list of words to store @ this is a list of words to look up",
      "'this' found\n'is' found\n'a' found\n'list' found\n'of' found\n'words' found\n'to' found\n'look' not found\n'up' not found0"
    )
  ]

exception MyError
let my_tests = [
  ("typechecked_bop",
  (fun () ->
      if (Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop(Mul,no_loc (CInt(2L)), no_loc (CInt(2L))))) = TInt)
      then ()
      else failwith "should not fail") )
; ("failed_typecheck_bop",
  fun () -> (try ( ( let x = Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop(Mul,no_loc (CInt(2L)), no_loc (CBool(true))))) in (if(x=TInt) then (raise MyError) else () ) ) )
   with  Typechecker.TypeError(_) -> () | MyError -> failwith "should not have passed"
      ) )
]

let unit_tests2 = [
  "type_bop_add",
  (fun () ->
  if (Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop(Add, no_loc (CInt(10L)), no_loc (CInt(10L))))) = TInt) then ()
      else failwith "should not fail")
; ("no_type_bop_add",
  (fun () ->
      try if(Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop(Mul,no_loc (CInt(10L)), no_loc (CStr("NaN"))))) = TInt) then failwith "should not pass" else ()
      with e -> ()
 ))
]

let unit_tests3 = [
  "subtype_fun_fun",
   (fun () ->
       if Typechecker.subtype_ref Tctxt.empty
         (RFun ([TBool; TInt; TNullRef RString], (RetVal TInt)))
         (RFun ([TBool; TInt; TNullRef RString], (RetVal TInt)))
        then ()
       else failwith "should not fail")
; ("no_subtype_fun_fun",
   (fun () ->
       if Typechecker.subtype_ref Tctxt.empty
         (RFun ([TBool; TInt; TNullRef RString], (RetVal TInt)))
         (RFun ([TBool; TInt; TNullRef RString], (RetVoid)))
        then failwith "should not succeed" else ())
  )
]

let str_tc =
  let tc = Tctxt.empty
  in let tc1 = Tctxt.add_struct tc "1" [{fieldName = "x1"; ftyp = TInt}; {fieldName = "x2"; ftyp = TInt}]
  in let tc2 = Tctxt.add_struct tc1 "2" [{fieldName = "x1"; ftyp = TInt}]
  in let tc3 = Tctxt.add_struct tc2 "3" [{fieldName = "x1"; ftyp = TInt}; {fieldName = "x2"; ftyp = TBool}]
  in tc3
let struct_tests = [
  "subtype_struct1_struct2",
   (fun () ->
       if Typechecker.subtype str_tc (TRef(RStruct("1"))) (TRef(RStruct("2"))) then ()
       else failwith "should not fail")
; ("no_subtype_struct1_struct3",
   (fun () ->
       if Typechecker.subtype str_tc (TRef(RStruct("1"))) (TRef(RStruct("3"))) then
         failwith "should not succeed" else ())
  )
]

let my_tests1 = [
  "good",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt)) then ()
       else failwith "should not fail")
; ("bad",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TBool)) then
         failwith "should not succeed" else ())
  )
]

let provided_tests : suite = [
  GradedTest("custom unit tests", 1, unit_tests);
  GradedTest("tetrate tests", 0, executed_oat_file tetrate_tests);
  GradedTest("subtype unit tests", 1, typecheck_unit_tests);
  GradedTest("Execution Tests", 1, executed_oat_file [
    ("doubly-linked-list.oat", "", "3")
  ]);
  GradedTest ("Ed's subtyping tests", 4, subtype_tests1);
  GradedTest ("Elliot's OAT trie test", 4, executed_oat_file studenttests1);
  GradedTest("my_unit_test", 0,my_tests);
  GradedTest("unit_test_cases", 0, unit_tests2);
  GradedTest("tri_num_test_case", 0, executed_oat_file [("tri_num.oat", "", "21")]);
  GradedTest("my_tests", 0, unit_tests3);
  GradedTest("Execution Tests", 0, executed_oat_file [("shapes.oat", "", "12")]);
  Test("Semiprime Test",
  (executed_oat_file [("semiprime.oat", "", "Semiprimes: 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 106 111 115 118 119 121 122 123 129 133 134 141 142 143 145 146 155 158 159 161 166 169 177 178 183 185 187 194 201 202 203 205 206 209 213 214 215 217 218 219 221 226 235 237 247 249 253 254 259 262 265 267 274 278 287 289 291 295 298 299 \nOthers: 2 3 5 7 8 11 12 13 16 17 18 19 20 23 24 27 28 29 30 31 32 36 37 40 41 42 43 44 45 47 48 50 52 53 54 56 59 60 61 63 64 66 67 68 70 71 72 73 75 76 78 79 80 81 83 84 88 89 90 92 96 97 98 99 100 101 102 103 104 105 107 108 109 110 112 113 114 116 117 120 124 125 126 127 128 130 131 132 135 136 137 138 139 140 144 147 148 149 150 151 152 153 154 156 157 160 162 163 164 165 167 168 170 171 172 173 174 175 176 179 180 181 182 184 186 188 189 190 191 192 193 195 196 197 198 199 200 204 207 208 210 211 212 216 220 222 223 224 225 227 228 229 230 231 232 233 234 236 238 239 240 241 242 243 244 245 246 248 250 251 252 255 256 257 258 260 261 263 264 266 268 269 270 271 272 273 275 276 277 279 280 281 282 283 284 285 286 288 290 292 293 294 296 297 0")]));
  Test("Typechecker struct tests", struct_tests);
  Test("ant test",
    executed_oat_file[("ant.oat", "", "0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 1 1 0 0 0 0
0 0 0 0 1 1 1 0 0 0 0
0 0 0 0 1 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0")]);
  GradedTest("my_unit_test1", 0, my_tests1);
]
