open Assert
open X86
open Simulator
open Gradedtests
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let power_iter n p = [
  text "main"
  [
    Movq, [~$n; ~%R08];
    Movq, [~$p; ~%R09];
    Cmpq, [~$0; ~%R09];
    J Eq, [~$$"zeropower"];
    Movq, [~%R08; ~%Rax]
  ];
  text "loop"
  [
    Subq, [~$1; ~%R09];
    Cmpq, [~$0; ~%R09];
    J Eq, [~$$"exit"];
    Imulq, [~%R08; ~%Rax];
    Jmp, [~$$"loop"]
  ];
  text "zeropower"
  [
    Movq, [~$1; ~%Rax];
    Jmp, [~$$"exit"]
  ];
  text "exit"
  [
    Retq, []
  ]
]



let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  ]);

  GradedTest ("Power", 10, [
    ("power1", program_test (power_iter 5 0) 1L);
  ]);
  GradedTest ("Power", 10, [
    ("power1", program_test (power_iter 5 3) 125L);
  ]);

]
