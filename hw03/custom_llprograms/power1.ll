define i64 @power(i64 %b, i64 %n) {
	%1 = icmp eq i64 %n, 0
	br i1 %1, label %end, label %recurse
recurse:
	%2 = sub i64 %n, 1
	%3 = call i64 @power(i64 %b, i64 %2)
	%4 = mul i64 %b, %3
	ret i64 %4
end:
	ret i64 1
}

define i64 @main(i64 %argc, i8** %arcv) {
	%1 = call i64 @power(i64 2, i64 2)
	ret i64 %1
}