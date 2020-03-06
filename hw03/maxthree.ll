
;   Implemented by Ramana Nagasamudram and Yuandong Liu


define i64 @max(i64 %0, i64 %1, i64 %2) {
  %4 = alloca i64 
  %5 = alloca i64 
  %6 = alloca i64 
  %7 = alloca i64 
  store i64 %0, i64* %5
  store i64 %1, i64* %6 
  store i64 %2, i64* %7 
  %8 = load i64, i64* %5 
  %9 = load i64, i64* %6 
  %10 = icmp sge i64 %8, %9
  br i1 %10, label %a1, label %a3

a1:                                     
  %12 = load i64, i64* %5
  %13 = load i64, i64* %7 
  %14 = icmp sge i64 %12, %13
  br i1 %14, label %a2, label %a3

a2:                                     
  %16 = load i64, i64* %5 
  store i64 %16, i64* %4
  br label %a7

a3:                                     
  %18 = load i64, i64* %6 
  %19 = load i64, i64* %5 
  %20 = icmp sge i64 %18, %19
  br i1 %20, label %a4, label %a6

a4:                                     
  %22 = load i64, i64* %6 
  %23 = load i64, i64* %7 
  %24 = icmp sge i64 %22, %23
  br i1 %24, label %a5, label %a6

a5:                                     
  %26 = load i64, i64* %6 
  store i64 %26, i64* %4
  br label %a7

a6:                                     
  %28 = load i64, i64* %7 
  store i64 %28, i64* %4
  br label %a7

a7:                                     
  %30 = load i64, i64* %4 
  ret i64 %30
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = alloca i64 
  %3 = alloca i64 
  %4 = alloca i64 
  store i64 0, i64* %1 
  store i64 11, i64* %2 
  store i64 22, i64* %3 
  store i64 26, i64* %4 
  %5 = load i64, i64* %2 
  %6 = load i64, i64* %3 
  %7 = load i64, i64* %4 
  %8 = call i64 @max(i64 %5, i64 %6, i64 %7)
  ret i64 %8
}


