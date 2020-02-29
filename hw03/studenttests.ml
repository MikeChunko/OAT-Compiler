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

let provided_tests : suite = [
	GradedTest ("icmp tests", 3, executed icmp_tests)
]
