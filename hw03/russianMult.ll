;Russian Multiplication implementation by Leo Ouyang
;I pledge my honor that I have abided by the Stevens Honor System

@b = global i64 7
@a = global i64 5

define i64 @halve(i64 %dividend){
    %q = alloca i64
    store i64 0,i64* %q
    %d = alloca i64
    store i64 %dividend, i64* %d
    br label %start
start:
    %l = load i64, i64* %d
   %1 = icmp slt i64 %l, 2
    br i1 %1, label %asdf, label %bsdf     
asdf:             
        %g = load i64, i64* %q
        ret i64 %g
bsdf:
    %2 = load i64, i64* %q
    %3 = add i64 %2, 1
    store i64 %3, i64* %q
    %4 = load i64, i64* %d
    %5 = sub i64 %4, 2
    store i64 %5, i64* %d  
      br label %start

}
define i64 @mod(i64 %dividend){
    %q = alloca i64
    store i64 0,i64* %q
    %d = alloca i64
    store i64 %dividend, i64* %d
    br label %start1
start1:
    %l = load i64, i64* %d
   %1 = icmp slt i64 %l, 2
    br i1 %1, label %asdf1, label %bsdf1     
asdf1:             
        %g = load i64, i64* %d
        ret i64 %g
bsdf1:
    %2 = load i64, i64* %q
    %3 = add i64 %2, 1
    store i64 %3, i64* %q
    %4 = load i64, i64* %d
    %5 = sub i64 %4, 2
    store i64 %5, i64* %d  
      br label %start1

}

define i64 @main(i64 %argc, i8** %arcv) {

    %accum = alloca i64
    store i64 0, i64* %accum 
    br label %peasant
peasant: ;checks if a is odd
    %alabama = load i64, i64* @a
    %3 = call i64 @mod(i64 %alabama)
    %5 = icmp eq i64 %3, 1
    br i1 %5, label %odd, label %even
odd: ;adds b to accumulator
    %truck = load i64, i64* %accum
    %jam = load i64, i64* @b
    %6 = add i64 %jam, %truck
    store i64 %6, i64* %accum
    
    %1 = load i64, i64* @a
    %2 = icmp eq i64 %1, 1
    br i1 %2, label %finish, label %even
even: ;updates a and b
    %bibba = load i64, i64* @b
    %apple = mul i64 %bibba, 2
    store i64 %apple, i64* @b
    %4 = call i64 @halve(i64 %alabama)
    store i64 %4, i64* @a
    br label %peasant
finish:
    %answer = load i64, i64* %accum
    ret i64 %answer
}
