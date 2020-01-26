open Assert
open Hellocaml

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 25) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) 64);
  ]);
  
  Test ("Problem4-4 Custom Tests", [
    ("optimize4", assert_eqf (fun () -> interpret ctxt1 (optimize e1)) 6L);
    ("optimize5", assert_eqf (fun () -> interpret ctxt1 (optimize e2)) 4L);
    ("optimize6", (fun () -> try ignore (interpret ctxt1 (optimize e3)); failwith "bad interpret" with Not_found -> ()));
    ("optimize7", assert_eqf (fun () -> optimize (Neg(Add(Const 0L, Mult(Const 0L, Var "x"))))) (Const 0L));
  ]);

  Test ("Problem5 Custom Tests", [
    ("compile1", assert_eqf (fun () -> run ctxt1 (compile e1)) 6L);
    ("compile2", assert_eqf (fun () -> run ctxt1 (compile e2)) 4L);
    ("compile3", (fun () -> try ignore (run ctxt1 (compile e3)); failwith "bad interpret" with Not_found -> ()));
  ]);
] 
