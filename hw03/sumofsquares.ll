; Euclidean algorithm for GCD by Jason Tran and Dan Thek

define i64 @sumofsquares(i64 %a){
    %1 = alloca i64
    %2 = alloca i64
    store i64 %a, i64* %1
    store i64 0, i64* %2
    %3 = icmp eq i64 %a, 0
    br i1 %3, label %return, label %loop
return:
    %4 = load i64, i64* %2
    ret i64 %4
loop:
    %5 = load i64, i64* %1
    %6 = load i64, i64* %2
    %7 = mul i64 %5, %5
    %8 = add i64 %7, %6
    store i64 %8, i64* %2
    %9 = sub i64 %5, 1
    store i64 %9, i64* %1
    %10 = icmp eq i64 %9, 0 ;%4 is 1 if a == 0, 0 otherwise
    br i1 %10, label %return, label %loop ;if %4 == 1, then return, else keep going
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @sumofsquares(i64 8)
  ret i64 %1
}

