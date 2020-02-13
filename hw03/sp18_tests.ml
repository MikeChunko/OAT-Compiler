open Assert
open X86
open Ll
open Backend

let tests =
  [ "llprograms/piazza_submissions/petrosky_branches.ll", 0L
  ; "llprograms/piazza_submissions/mannd_quicksort.ll", 1L
  ; "llprograms/piazza_submissions/anon1_bubblesort.ll", 1L
  ; "llprograms/piazza_submissions/geyerj_recursive_gcd.ll", 33L
  ; "llprograms/piazza_submissions/geyerj_call_many_args.ll", 38L
  ; "llprograms/piazza_submissions/kathdix_sumlist.ll", 15L
  ; "llprograms/piazza_submissions/andreasx_selectionsort.ll", 1L
  ; "llprograms/piazza_submissions/xiaov_gep.ll", 6L
  ; "llprograms/piazza_submissions/sakhavan_somilgo_bst_traversal.ll", 1L
  ; "llprograms/piazza_submissions/aen_prime_number.ll", 229L
  ; "llprograms/piazza_submissions/emsu_toposort_dfs.ll", 1L
  ; "llprograms/piazza_submissions/wglisson_mat_multiply.ll", 173L
  ; "llprograms/piazza_submissions/emsu_toposort_dfs.ll", 1L
  ; "llprograms/piazza_submissions/atter_fib_min_even.ll", 12L
  ]
