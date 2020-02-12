(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
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

let tetrate_rec a n = [
  text "main" [
    Movq, [~$a; ~%R08];
    Movq, [~$n; ~%R09];
    Addq, [~$1; ~%R09];
    Callq, [~$$"tetrate"];

    Retq, []
  ];
  (* Computes power given base and exponent, ends with original values back in R08 and R09, with return in Rax *)
  text "power"
  [
    Pushq, [~%R08];
    Pushq, [~%R09];
    Cmpq, [~$0; ~%R09];
    J Eq, [~$$"zeropower"];
    Movq, [~%R08; ~%Rax]
  ];
  text "loop"
  [
    Subq, [~$1; ~%R09];
    Cmpq, [~$0; ~%R09];
    J Eq, [~$$"exit_rec"];
    Imulq, [~%R08; ~%Rax];
    Jmp, [~$$"loop"]
  ];
  text "zeropower"
  [
    Movq, [~$1; ~%Rax];
    Jmp, [~$$"exit_rec"]
  ];
  text "exit_rec"
  [
    Popq, [~%R09];
    Popq, [~%R08];
    Retq, []
  ];
  (* Tetrate Code Begins *)
  text "end_tetrate"
  [
    Movq, [~$1; ~%Rax];
    Jmp, [~$$"exit_rec"]
  ];
  text "tetrate"
  [
    Pushq, [~%R08];
    Pushq, [~%R09];
    Subq, [~$1; ~%R09]; (* Need to make the initial call NOT subtract *)
    Cmpq, [~$0; ~%R09];
    J Eq, [~$$"end_tetrate"];

    Callq, [~$$"tetrate"];
    Movq, [~%Rax; ~%R09];
    Callq, [~$$"power"]; (* The result will now be in Rax *)
    Jmp, [~$$"exit_rec"]
  ];
]

let array_sum = [
  text "main"
  [
    Movq, [~$1; Ind3 (Lit (-48L), Rsp)];
    Movq, [~$2; Ind3 (Lit (-40L), Rsp)];
    Movq, [~$3; Ind3 (Lit (-32L), Rsp)];
    Movq, [~$4; Ind3 (Lit (-24L), Rsp)];
    Movq, [~$5; Ind3 (Lit (-16L), Rsp)];

    Movq, [~%Rsp; ~%R08];
    Addq, [~$(-48); ~%R08];

    Movq, [~%Rsp; ~%R09];
    Subq, [~$8; ~%R09];
    Movq, [~$0; ~%Rax]
  ];
  text "loop"
  [
    Cmpq, [~%R08; ~%R09];
    J Eq, [~$$"exit"];

    Addq, [Ind3 (Lit (0L), R08); ~%Rax];
    Addq, [~$8; ~%R08];
    Jmp, [~$$"loop"];
  ];
  text "exit"
  [
    Retq, []
  ]
]


let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  ]);

  GradedTest ("Power_Zero", 10, [
    ("power1", program_test (power_iter 5 0) 1L);
  ]);
  GradedTest ("Power_Normal", 10, [
    ("power2", program_test (power_iter 5 3) 125L);
  ]);
  GradedTest ("Tetrate_Rec", 10, [
    ("tetrate1", program_test (tetrate_rec 2 4) 65536L);
  ]);
  GradedTest ("Array_Test", 10, [
    ("array1", program_test (array_sum) 15L);
  ]);

]
