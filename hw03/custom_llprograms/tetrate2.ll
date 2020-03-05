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

define i64 @tetrate(i64 %b, i64 %n) {
	%1 = icmp eq i64 %n, 0
	br i1 %1, label %end1, label %recurse1
recurse1:
	%2 = sub i64 %n, 1
	%3 = call i64 @tetrate(i64 %b, i64 %2)
	%4 = call i64 @power(i64 %b, i64 %3)
	ret i64 %4
end1:
	ret i64 1
}

define i64 @main(i64 %argc, i8** %arcv) {
	%1 = call i64 @tetrate(i64 2, i64 0)
	ret i64 %1
}