%node = type { i64, %node* }

@list1 = global %node { i64 2, %node* @l1node2 }
@l1node2 = global %node { i64 3, %node* @list1 }

@list2 = global %node { i64 64, %node* @list2 }

@list3 = global %node { i64 -24, %node* @l3node2 }
@l3node2 = global %node { i64 -16, %node* @l3node3 }
@l3node3 = global %node { i64 -8, %node* @l3node4 }
@l3node4 = global %node { i64 0, %node* @l3node5 }
@l3node5 = global %node { i64 0, %node* @l3node6 }
@l3node6 = global %node { i64 8, %node* @l3node7 }
@l3node7 = global %node { i64 16, %node* @l3node8 }
@l3node8 = global %node { i64 32, %node* @l3node9 }
@l3node9 = global %node { i64 40, %node* @l3node10 }
@l3node10 = global %node { i64 48, %node* @list3 }

@list4 = global %node { i64 -24, %node* @l4node2 }
@l4node2 = global %node { i64 -16, %node* @l4node3 }
@l4node3 = global %node { i64 -8, %node* @l4node4 }
@l4node4 = global %node { i64 0, %node* @l4node5 }
@l4node5 = global %node { i64 0, %node* @l4node6 }
@l4node6 = global %node { i64 8, %node* @l4node7 }
@l4node7 = global %node { i64 16, %node* @l4node8 }
@l4node8 = global %node { i64 32, %node* @l4node9 }
@l4node9 = global %node { i64 40, %node* @l4node10 }
@l4node10 = global %node { i64 48, %node* @l4node4 }

@list5 = global %node { i64 -24, %node* @l5node2 }
@l5node2 = global %node { i64 -16, %node* @l5node3 }
@l5node3 = global %node { i64 -8, %node* @l5node4 }
@l5node4 = global %node { i64 0, %node* @l5node5 }
@l5node5 = global %node { i64 0, %node* @l5node6 }
@l5node6 = global %node { i64 8, %node* @l5node7 }
@l5node7 = global %node { i64 16, %node* @l5node8 }
@l5node8 = global %node { i64 32, %node* @l5node9 }
@l5node9 = global %node { i64 40, %node* @l5node10 }
@l5node10 = global %node { i64 48, %node* @l5node10 }

@list6 = global %node { i64 -24, %node* @l6node2 }
@l6node2 = global %node { i64 -16, %node* @l6node3 }
@l6node3 = global %node { i64 -8, %node* @l6node4 }
@l6node4 = global %node { i64 0, %node* @l6node5 }
@l6node5 = global %node { i64 0, %node* @l6node6 }
@l6node6 = global %node { i64 8, %node* @l6node7 }
@l6node7 = global %node { i64 16, %node* @l6node8 }
@l6node8 = global %node { i64 32, %node* @l6node9 }
@l6node9 = global %node { i64 40, %node* @l6node10 }
@l6node10 = global %node { i64 48, %node* @l6node5 }


@list8 = global %node { i64 64, %node* null }

@list9 = global %node { i64 2, %node* @l9node2 }
@l9node2 = global %node { i64 3, %node* null }

@list10 = global %node { i64 -24, %node* @l10node2 }
@l10node2 = global %node { i64 -16, %node* @l10node3 }
@l10node3 = global %node { i64 -8, %node* @l10node4 }
@l10node4 = global %node { i64 0, %node* @l10node5 }
@l10node5 = global %node { i64 0, %node* @l10node6 }
@l10node6 = global %node { i64 8, %node* @l10node7 }
@l10node7 = global %node { i64 16, %node* @l10node8 }
@l10node8 = global %node { i64 32, %node* @l10node9 }
@l10node9 = global %node { i64 40, %node* @l10node10 }
@l10node10 = global %node { i64 48, %node* null }

@list11 = global %node { i64 64, %node* @list10 }


define i64 @has_cycle(%node* %head) {
  
  %tmp = icmp eq %node* %head, null
  br i1 %tmp, label %hit_null, label %setup

setup:
  %slow = alloca %node*
  %fast = alloca %node*
  store %node* %head, %node** %slow 

  %1 = getelementptr %node, %node* %head, i32 0, i32 1
  %2 = load %node*, %node** %1 
  store %node* %2, %node** %fast
  br label %check_fast
hit_null:
  ret i64 0
found_cycle:
  ret i64 1
check_fast:
  %3 = load %node*, %node** %fast
  %4 = icmp eq %node* %3, null
  br i1 %4, label %hit_null, label %check_fast_next
check_fast_next:
  %5 = load %node*, %node** %fast
  %6 = getelementptr %node, %node* %5, i32 0, i32 1
  %7 = load %node*, %node** %6
  store %node* %7, %node** %fast
  %8 = icmp eq %node* %7, null
  br i1 %8, label %hit_null, label %advance_ptrs
advance_ptrs:
  %9 = load %node*, %node** %slow
  %10 = getelementptr %node, %node* %9, i32 0, i32 1
  %11 = load %node*, %node** %10
  store %node* %11, %node** %slow

  %12 = load %node*, %node** %fast
  %13 = getelementptr %node, %node* %12, i32 0, i32 1
  %14 = load %node*, %node** %13
  store %node* %14, %node** %fast
  br label %check_eq
check_eq:
  %15 = load %node*, %node** %slow
  %16 = load %node*, %node** %fast 
  %17 = icmp eq %node* %15, %16
  br i1 %17, label %found_cycle, label %check_fast
}


define i64 @main(i64 %argc, i8** %arcv) {
  %accum = alloca i64
  store i64 0, i64* %accum

  %1 = call i64 @has_cycle(%node* @list1)
  %2 = load i64, i64* %accum
  %3 = add i64 %1, %2
  store i64 %3, i64* %accum

  %4 = call i64 @has_cycle(%node* @list2)
  %5 = load i64, i64* %accum
  %6 = add i64 %4, %5
  store i64 %6, i64* %accum

  %7 = call i64 @has_cycle(%node* @list3)
  %8 = load i64, i64* %accum
  %9 = add i64 %7, %8
  store i64 %9, i64* %accum

  %10 = call i64 @has_cycle(%node* @list4)
  %11 = load i64, i64* %accum
  %12 = add i64 %10, %11
  store i64 %12, i64* %accum

  %13 = call i64 @has_cycle(%node* @list5)
  %14 = load i64, i64* %accum
  %15 = add i64 %13, %14
  store i64 %15, i64* %accum

  %16 = call i64 @has_cycle(%node* @list6)
  %17 = load i64, i64* %accum
  %18 = add i64 %16, %17
  store i64 %18, i64* %accum

  %19 = call i64 @has_cycle(%node* null)
  %20 = load i64, i64* %accum
  %21 = add i64 %19, %20
  store i64 %21, i64* %accum

  %22 = call i64 @has_cycle(%node* @list8)
  %23 = load i64, i64* %accum
  %24 = add i64 %22, %23
  store i64 %24, i64* %accum

  %25 = call i64 @has_cycle(%node* @list10)
  %26 = load i64, i64* %accum
  %27 = add i64 %25, %26
  store i64 %27, i64* %accum

  %28 = call i64 @has_cycle(%node* @list11)
  %29 = load i64, i64* %accum
  %30 = add i64 %28, %29
  store i64 %30, i64* %accum

  %31 = call i64 @has_cycle(%node* @list9)
  %32 = load i64, i64* %accum
  %33 = add i64 %31, %32
  store i64 %33, i64* %accum


  %result = load i64, i64* %accum
  ret i64 %result
}
