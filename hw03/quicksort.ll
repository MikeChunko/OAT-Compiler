; Quicksort implementation by Adam Chen and Kurt Louie

@myarray = global [8 x i64] [i64 5, i64 7, i64 6, i64 4, i64 3, i64 2, i64 0, i64 1]

define i64 @partition([8 x i64]* %a, i64 %lo, i64 %hi) {
    %1 = add i64 %lo, %hi
    %2 = lshr i64 %1, 1
    %pivotptr = getelementptr [8 x i64], [8 x i64]* %a, i64 0, i64 %2
    %pivot = load i64, i64* %pivotptr
    %iptr = alloca i64
    %jptr = alloca i64
    %i_init = sub i64 %lo, 1
    %j_init = add i64 %hi, 1
    store i64 %i_init, i64* %iptr
    store i64 %j_init, i64* %jptr
    br label %i_loop
    
i_loop:
    %i = load i64, i64* %iptr
    %newi = add i64 %i, 1
    store i64 %newi, i64* %iptr
    
    %ai_ptr = getelementptr [8 x i64], [8 x i64]* %a, i64 0, i64 %newi
    %ai = load i64, i64* %ai_ptr
    %iguard = icmp slt i64 %ai, %pivot
    br i1 %iguard, label %i_loop, label %j_loop

j_loop:
    %j = load i64, i64* %jptr
    %newj = sub i64 %j, 1
    store i64 %newj, i64* %jptr
    
    %aj_ptr = getelementptr [8 x i64], [8 x i64]* %a, i64 0, i64 %newj
    %aj = load i64, i64* %aj_ptr
    %jguard = icmp sgt i64 %aj, %pivot
    br i1 %jguard, label %j_loop, label %post_loop
    
post_loop:
    %icur = load i64, i64* %iptr
    %jcur = load i64, i64* %jptr
    %guard = icmp sge i64 %icur, %jcur
    br i1 %guard, label %par_exit, label %swap

par_exit:
    %jret = load i64, i64* %jptr
    ret i64 %jret

swap:
    %i_swap = load i64, i64* %iptr
    %j_swap = load i64, i64* %jptr
    %ai_swap_ptr = getelementptr [8 x i64], [8 x i64]* %a, i64 0, i64 %i_swap
    %aj_swap_ptr = getelementptr [8 x i64], [8 x i64]* %a, i64 0, i64 %j_swap
    %temp = load i64, i64* %ai_swap_ptr
    %temp2 = load i64, i64* %aj_swap_ptr
    store i64 %temp, i64* %aj_swap_ptr
    store i64 %temp2, i64* %ai_swap_ptr
    br label %i_loop
    
}

define void @quicksort([8 x i64]* %a, i64 %lo, i64 %hi) {
    %1 = icmp slt i64 %lo, %hi
    br i1 %1, label %qs_rec, label %qs_exit

qs_rec:
    %p = call i64 @partition([8 x i64]* %a, i64 %lo, i64 %hi)
    call void @quicksort([8 x i64]* %a, i64 %lo, i64 %p)
    %pinc = add i64 1, %p
    call void @quicksort([8 x i64]* %a, i64 %pinc, i64 %hi)
    ret void

qs_exit:
    ret void
}

define i64 @main(i64 %argc, i8** %arcv) {
    call void @quicksort([8 x i64]* @myarray, i64 0, i64 7)
    
    %z_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 0
    %z = load i64, i64* %z_ptr
    %zprime = mul i64 %z, 0
    %o_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 1
    %o = load i64, i64* %o_ptr
    %oprime = mul i64 %o, 1
    %t_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 2
    %t = load i64, i64* %t_ptr
    %tprime = mul i64 %t, 2
    %th_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 3
    %th = load i64, i64* %th_ptr
    %thprime = mul i64 %th, 3
    %f_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 4
    %f = load i64, i64* %f_ptr
    %fprime = mul i64 %f, 4
    %fi_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 5
    %fi = load i64, i64* %fi_ptr
    %fiprime = mul i64 %fi, 5
    %s_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 6
    %s = load i64, i64* %s_ptr
    %sprime = mul i64 %s, 6
    %se_ptr = getelementptr [8 x i64], [8 x i64]* @myarray, i64 0, i64 7
    %se = load i64, i64* %se_ptr
    %seprime = mul i64 %se, 7
    
    %acc1 = add i64 %zprime, %oprime
    %acc2 = add i64 %acc1, %tprime
    %acc3 = add i64 %acc2, %thprime
    %acc4 = add i64 %acc3, %fprime
    %acc5 = add i64 %acc4, %fiprime
    %acc6 = add i64 %acc5, %sprime
    %acc7 = add i64 %acc6, %seprime
    
    ret i64 %acc7
}
