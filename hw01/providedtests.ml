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

  Test ("Problem4-3 Custom Tests", [
      ("interpret1", assert_eqf (fun () -> interpret ctxt3 e4) 1L);
      ("interpret2", assert_eqf (fun () -> interpret ctxt3 e5) 9L);
      ("interpret3", (fun () -> try ignore (interpret ctxt3 e6); failwith "bad interpret" with Not_found -> ()));
  ]);
  
  Test ("Problem4-4 Custom Tests", [
    ("optimize1", assert_eqf (fun () -> interpret ctxt1 (optimize e1)) 6L);
    ("optimize2", assert_eqf (fun () -> interpret ctxt1 (optimize e2)) 4L);
    ("optimize3", (fun () -> try ignore (interpret ctxt1 (optimize e3)); failwith "bad interpret" with Not_found -> ()));
    ("optimize4", assert_eqf (fun () -> optimize (Neg(Add(Const 0L, Mult(Const 0L, Var "x"))))) (Const 0L));
    ("optimize5", assert_eqf (fun () -> interpret ctxt3 (optimize e4)) 1L);
    ("optimize6", assert_eqf (fun () -> interpret ctxt3 (optimize e5)) 9L);
    ("optimize7", (fun () -> try ignore (interpret ctxt3 (optimize e6)); failwith "bad interpret" with Not_found -> ()));
  ]);

  Test ("Problem5 Custom Tests", [
    ("compile1", assert_eqf (fun () -> run ctxt1 (compile e1)) 6L);
    ("compile2", assert_eqf (fun () -> run ctxt1 (compile e2)) 4L);
    ("compile3", (fun () -> try ignore (run ctxt1 (compile e3)); failwith "bad interpret" with Not_found -> ()));
    ("compile5", assert_eqf (fun () -> run ctxt3 (compile e4)) 1L);
    ("compile6", assert_eqf (fun () -> run ctxt3 (compile e5)) 9L);
    ("compile7", (fun () -> try ignore (run ctxt3 (compile e6)); failwith "bad interpret" with Not_found -> ()));
  ]);
] 
