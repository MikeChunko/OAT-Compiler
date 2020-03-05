(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let icmp_tests = 
[ "llprograms/icmp1.ll", 1L
; "llprograms/icmp2.ll", 0L
; "llprograms/icmp3.ll", 1L ]

let tetrate_tests = 
[ "custom_llprograms/power1.ll", 4L
; "custom_llprograms/power2.ll", 1L
; "custom_llprograms/power3.ll", 81L 
; "custom_llprograms/tetrate1.ll", 16L
; "custom_llprograms/tetrate2.ll", 1L
; "custom_llprograms/tetrate3.ll", 27L
; "custom_llprograms/tetrate4.ll", 4L
]

let provided_tests : suite = [
  GradedTest ("icmp tests", 0, executed icmp_tests);
  GradedTest ("tetrate_tests", 0, executed tetrate_tests)
]
