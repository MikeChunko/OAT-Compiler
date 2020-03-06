define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = alloca i64
  store i64 0, i64* %1
  store i64 3, i64* %2
  br label %gettri
gettri:
  %3 = load i64, i64* %1
  %4 = load i64, i64* %2
  %5 = add i64 %3, %4
  store i64 %5, i64* %1
  %6 = sub i64 %4, 1
  store i64 %6, i64* %2
  %7 = icmp sge i64 0, %6
  br i1 %7, label %exit, label %gettri
exit: 
  %8 = load i64, i64* %1
  ret i64 %8
}