%struct.List = type { i64, %struct.List*}

@list9 = global %struct.List { i64 7, %struct.List* null }
@list8 = global %struct.List { i64 2, %struct.List* @list9 }
@list7 = global %struct.List { i64 1, %struct.List* @list8 }
@list6 = global %struct.List { i64 8, %struct.List* @list7 }
@list5 = global %struct.List { i64 9, %struct.List* @list6 }
@list4 = global %struct.List { i64 5, %struct.List* @list5 }
@list3 = global %struct.List { i64 10, %struct.List* @list4 }
@list2 = global %struct.List { i64 3, %struct.List* @list3 }
@list1 = global %struct.List { i64 6, %struct.List* @list2 }

define i64 @whats_missing() {
  %i = alloca i64
  %n = alloca i64
  %lst1 = load %struct.List, %struct.List* @list1
  %lst = alloca %struct.List
  store %struct.List %lst1, %struct.List* %lst
  br label %loop

loop:
  %1 = getelementptr %struct.List, %struct.List* %lst, i32 0, i32 0
  %2 = getelementptr %struct.List, %struct.List* %lst, i32 0, i32 1
  %3 = load i64, i64* %1


  %count = load i64, i64* %i
  %a = add i64 1, %count
  store i64 %a, i64* %i

  %sum = load i64, i64* %n
  %r = add i64 %3, %count
  store i64 %r, i64* %n

  %next = load %struct.List*, %struct.List** %2
  %nnext = load %struct.List, %struct.List* %next
  store %struct.List %nnext, %struct.List* %lst
  

  %check = load i64, i64* %i
  %cmp = icmp sgt i64 %check, 10
  br i1 %cmp, label %loop, label %end

end:
  %sub = load i64, i64* %n
  %4 = sub i64 55, %sub

  ret i64 %4
}

define i64 @main(i64 %argc, i8** %argv) {
  %1 = call i64 @whats_missing() 
  ret i64 %1
}

