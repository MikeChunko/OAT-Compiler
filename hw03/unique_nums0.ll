; Unique numbers by Anthony Orrico and Justin Westley
; Returns True if numbers in array are unique and false if they are not

@icount = global i64 0
@ival = global i64 0
@jval = global i64 0
@jcount = global i64 1
@jcounter = global i64 1
@icheck = global i1 0
@list = global [10 x i64] [i64 10, i64 69, i64 10, i64 11, i64 64, i64 8, i64 7, i64 120, i64 40, i64 42]

define void @icountcheck(i64 %ic){
  %check = icmp eq i64 %ic, 10
  br i1 %check, label %p, label %cont

cont:
  ret void

p:
  store i1 1, i1* @icheck
  ret void
}

define i64 @main(i64 %argc, i8** %argv) {
  %input = getelementptr [10 x i64], [10 x i64]* @list, i64 0, i64 0
  %iv = load i64, i64* %input
  store i64 %iv, i64* @ival

  %j = getelementptr [10 x i64], [10 x i64]* @list, i64 0, i64 1
  %jv = load i64, i64* %j
  store i64 %jv, i64* @jval

  br label %unique

unique:
  %ivalcomp = load i64, i64* @ival
  %jvalcomp = load i64, i64* @jval
  %checker = icmp eq i64 %ivalcomp, %jvalcomp
  br i1 %checker, label %failed, label %jloop

ibattle:
  %ibattleval = load i64, i64* @icount
  %ib = add i64 %ibattleval, 1
  call void @icountcheck(i64 %ib)
  %z = load i1, i1* @icheck
  br i1 %z, label %passed, label %iloop

iloop: 
  %xd = load i64, i64* @icount
  %newicount = add i64 %xd, 1
  store i64 %newicount, i64* @icount
  %newi = getelementptr [10 x i64], [10 x i64]* @list, i64 0, i64 %newicount
  %iv2 = load i64, i64* %newi
  store i64 %iv2, i64* @ival
 
  %abc = load i64, i64* @jcount
  %newjcount = add i64 %abc, 1
  store i64 %newjcount, i64* @jcount
  %newj = getelementptr [10 x i64], [10 x i64]* @list, i64 0, i64 %newjcount
  %jv2 = load i64, i64* %newj
  store i64 %jv2, i64* @jval
  store i64 %newjcount, i64* @jcounter

  br label %unique

jloop:
  %cntr = load i64, i64* @jcounter
  %newjcount2 = add i64 %cntr, 1
  store i64 %newjcount2, i64* @jcounter
  %newj2 = getelementptr [10 x i64], [10 x i64]* @list, i64 0, i64 %newjcount2
  %jv3 = load i64, i64* %newj2
  store i64 %jv3, i64* @jval

  %outofbounds = icmp slt i64 %newjcount2, 10
  br i1 %outofbounds, label %unique, label %ibattle

failed:
  ret i64 0

passed:
  ret i64 1
}