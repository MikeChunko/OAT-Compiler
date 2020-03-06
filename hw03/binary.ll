;Robert Schabacker and Jose Amezquita
;I pledge my honor that I have abided by the Stevens Honor System.
;prints out argc in binary

declare void @ll_putchar(i64)

define void @printBinary(i64 %n){
	%isZero = icmp eq i64 %n, 0
	br i1 %isZero, label %done, label %continue
done:
	ret void
continue:
	%1 = lshr i64 %n, 1
	call void @printBinary(i64 %1)
	%2 = and i64 %n, 1
	%3 = add i64 %2, 48
	call void @ll_putchar(i64 %3)
	ret void
}

define i64 @main(i64 %argc, i8** %argv){
	call void @printBinary(i64 %argc)
	call void @ll_putchar(i64 10) ;newline
	ret i64 0
}
