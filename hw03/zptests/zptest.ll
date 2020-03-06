@blank = global [1 x i8] c"\00"

define i64 @main(i64 %argc, i8** %argv) {
    %xstrptr = getelementptr i8*, i8** %argv, i32 1
    %xstr = load i8*, i8** %xstrptr
    ;call void @ll_puts(i8* @blank)
    %x = call i64 @ll_atoi(i8* %xstr)
    ;;%x = add i64 0, 1

    %outval = call i64 @horner(i64 %argc, i8** %argv, i64 %x, i64 2, i64 0)
    call void @ll_printint(i64 %outval)
    ret i64 0
}

define i64 @horner(i64 %arc, i8** %arv, i64 %x, i64 %i, i64 %acc){
    %cmpval = icmp sge i64 %i, %arc
    br i1 %cmpval, label %exit, label %recurse
exit:
    ret i64 %acc
recurse:
    %lptr1 = getelementptr i8*, i8** %arv,  i64 %i
    %costr = load i8*, i8** %lptr1 
    %coeff = call i64 @ll_atoi(i8* %costr)
    ;;;%coeff = add i64 %arc, 0

    %accmul = mul i64 %acc, %x
    %newacc = add i64 %coeff, %accmul
    %newi = add i64 %i, 1
    %retval = call i64 @horner(i64 %arc, i8** %arv, i64 %x, i64 %newi, i64 %newacc)
    ret i64 %retval
}