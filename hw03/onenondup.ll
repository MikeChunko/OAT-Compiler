%arr = type [9 x i64]

@tmp = global %arr [ i64 1, i64 2, i64 3, i64 4, i64 5 , i64 4, i64 3, i64 2, i64 1 ]

define i64 @nondup(i64 %n, i64 %val, i64* %a) {
    %1 = alloca i64
    %2 = getelementptr %arr, %arr* @tmp, i64 0, i64 %n
    %3 = load i64, i64* %2
    %4 = xor i64 %3, %val
    %cmp = icmp eq i64 %n, 0
    br i1 %cmp, label %ret1, label %recurse
ret1:
    ret i64 %4
recurse:
    %5 = sub i64 %n, 1
    %6 = call i64 @nondup(i64 %5, i64 %4, i64* %a)
    ret i64 %6
}

define i64 @main(i64 %argc, i8** %arcv) {
    %1 = call i64 @nondup(i64 9, i64 0, i64* @tmp)
    ret i64 %1
}

