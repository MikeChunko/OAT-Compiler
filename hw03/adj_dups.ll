; Test case for Alex Schlumpf and David Horowitz
; Program to check for adjacent duplicates
; in a linked list.
;
; The expected output to the program is 1, which indicates
; that all test cases passed.
; 
; l1: no adj duplicates
; l2: adj duplicates
; l3: no adj duplicates

%struct.ListNode = type { %struct.ListNode*, i64 }

@l1_node1 = global %struct.ListNode { %struct.ListNode* @l1_node2, i64 2 }
@l1_node2 = global %struct.ListNode { %struct.ListNode* @l1_node3, i64 -4 }
@l1_node3 = global %struct.ListNode { %struct.ListNode* @l1_node4, i64 1 }
@l1_node4 = global %struct.ListNode {  %struct.ListNode* null, i64 9 }

@l2_node1 = global %struct.ListNode { %struct.ListNode* @l2_node2, i64 2 }
@l2_node2 = global %struct.ListNode { %struct.ListNode* @l2_node3, i64 -4 }
@l2_node3 = global %struct.ListNode { %struct.ListNode* @l2_node4, i64 4 }
@l2_node4 = global %struct.ListNode {  %struct.ListNode* @l2_node5, i64 9 }
@l2_node5 = global %struct.ListNode {  %struct.ListNode* @l2_node6, i64 9 }
@l2_node6 = global %struct.ListNode {  %struct.ListNode* null, i64 15 }

@l3_node1 = global %struct.ListNode { %struct.ListNode* null, i64 2 }

define i64 @adj_dups(%struct.ListNode* %head) {
  %1 = getelementptr %struct.ListNode, %struct.ListNode* %head, i32 0, i32 1
  %2 = getelementptr %struct.ListNode, %struct.ListNode* %head, i32 0, i32 0
  %3 = load %struct.ListNode*, %struct.ListNode** %2
  %4 = icmp eq %struct.ListNode* %3, null
  br i1 %4, label %no_dups, label %val_cmp
  val_cmp:
    %5 = getelementptr %struct.ListNode, %struct.ListNode* %3, i32 0, i32 1
    %6 = load i64, i64* %1
    %7 = load i64, i64* %5
    %8 = icmp eq i64 %6, %7
    br i1 %8, label %dups, label %continue
  continue:
    %9 = load %struct.ListNode*, %struct.ListNode** %2
    %10 = call i64 @adj_dups(%struct.ListNode* %9)
    ret i64 %10
  no_dups:
    ret i64 0
  dups:
    ret i64 1

}

define i64 @main(i64 %argc, i8** %argv) {
  %1 = call i64 @adj_dups(%struct.ListNode* @l1_node1)
  %2 = call i64 @adj_dups(%struct.ListNode* @l2_node1)
  %3 = call i64 @adj_dups(%struct.ListNode* @l3_node1)

  %4 = icmp ne i64 %1, 0
  br i1 %4, label %fail, label %case2
  case2:
    %5 = icmp ne i64 %2, 1
    br i1 %5, label %fail, label %case3
  case3:
    %6 = icmp ne i64 %3, 0
    br i1 %6, label %fail, label %pass
  pass:
    ret i64 1
  fail:
    ret i64 0
}