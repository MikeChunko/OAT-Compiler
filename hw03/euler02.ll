;
; Project Euler Problem # 002
; 
; PROBLEM DESCRIPTION:
;
; By considering the terms in the Fibonacci sequence whose values 
; do not exceed four million, find the sum of the even-valued terms.
;
; Note: for this class we will use 500 instead (to keep result
; within a byte)
;
; correct answer: 188

define i64 @fibonacci(i64 %n) {
  %cmp = icmp sle i64 %n, 1
  br i1 %cmp, label %retn, label %recurse
retn:
  ret i64 %n
recurse:
  %1 = sub i64 %n, 1
  %2 = sub i64 %n, 2
  %3 = call i64 @fibonacci(i64 %1)
  %4 = call i64 @fibonacci(i64 %2)
  %5 = add i64 %3, %4 
  ret i64 %5
}

define i64 @solve() {
  %i = alloca i64
  %sum = alloca i64
  store i64 0, i64* %i
  store i64 0, i64* %sum
  br label %start
start:
  %0 = load i64, i64* %i
  %1 = add i64 1, %0
  store i64 %1, i64* %i
  %fib = call i64 @fibonacci(i64 %1)
  %3 = icmp sgt i64 %fib, 500
  br i1 %3, label %end, label %then
then:
  %cmp = and i64 1, %fib
  br i1 %cmp, label %start, label %even
even:
  %4 = load i64, i64* %sum 
  %5 = add i64 %fib, %4
  store i64 %5, i64* %sum
  br label %start
end:
  %6 = load i64, i64* %sum 
  ret i64 %6
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @solve()
  ret i64 %1
}
