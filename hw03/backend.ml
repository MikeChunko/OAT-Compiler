(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
(* ll ir compilation -------------------------------------------------------- *)
open Ll
open X86

(* Overview ----------------------------------------------------------------- *)

(* We suggest that you spend some time understinging this entire file and
  how it fits with the compiler pipeline before making changes.  The suggested
  plan for implementing the compiler is provided on the project web page. *)

(* Helpers ------------------------------------------------------------------ *)

(* Map LL comparison operations to X86 condition codes *)
let compile_cnd = function
  | Ll.Eq  -> X86.Eq
  | Ll.Ne  -> X86.Neq
  | Ll.Slt -> X86.Lt
  | Ll.Sle -> X86.Le
  | Ll.Sgt -> X86.Gt
  | Ll.Sge -> X86.Ge

(* Locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
  identifiers to X86 abstractions.  For the best performance, one
  would want to use an X86 register for each LLVM %uid.  However,
  since there are an unlimited number of %uids and only 16 registers,
  doing so effectively is quite difficult.  We will see later in the
  course how _register allocation_ algorithms can do a good job at
  this.

  A simpler, but less performant, implementation is to map each %uid
  in the LLVM source to a _stack slot_ (i.e. a region of memory in
  the stack).  Since LLVMlite, unlike real LLVM, permits %uid locals
  to store only 64-bit data, each stack slot is an 8-byte value.

  [ NOTE: For compiling LLVMlite, even i1 data values should be
  represented as a 8-byte quad. This greatly simplifies code
  generation. ]

  We call the datastructure that maps each %uid to its stack slot a
  'stack layout'.  A stack layout maps a uid to an X86 operand for
  accessing its contents.  For this compilation strategy, the operand
  is always an offset from ebp (in bytes) that represents a storage slot in
  the stack. *)
type layout = (uid * X86.operand) list

(* A context contains the global type declarations (needed for getelementptr
   calculations) and a stack layout. *)
type ctxt = { tdecls : (tid * ty) list
            ; layout : layout
            }

(* Useful for looking up items in tdecls or layouts *)
let lookup m x = List.assoc x m

(* Compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

  LL local %uids live in stack slots, whereas global ids live at
  global addresses that must be computed from a label.  Constants are
  immediately available, and the operand Null is the 64-bit 0 value.

    NOTE: two important facts about global identifiers:

    (1) You should use (Platform.mangle gid) to obtain a string
    suitable for naming a global label on your platform (OS X expects
    "_main" while linux expects "main").

    (2) 64-bit assembly labels are not allowed as immediate operands.
    That is, the X86 code: movq _gid %rax which looks like it should
    put the address denoted by _gid into %rax is not allowed.
    Instead, you need to compute an %rip-relative address using the
    leaq instruction:   leaq _gid(%rip).

  One strategy for compiling instruction operands is to use a
  designated register (or registers) for holding the values being
  manipulated by the LLVM IR instruction. You might find it useful to
  implement the following helper function, whose job is to generate
  the X86 instruction that moves an LLVM operand into a designated
  destination (usually a register). *)
let compile_operand ctxt dest : Ll.operand -> ins = function
  | Null    -> (Movq, [Imm (Lit 0L); dest])
  | Const i -> (Movq, [Imm (Lit i); dest])
  | Gid gid -> (Movq, [Ind3 (Lbl gid, Rip); dest])
  | Id uid  -> (Movq, [lookup ctxt.layout uid; dest])

(* Compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that
  generates code for the LLVM IR call instruction.

  The code you generate should follow the x64 System V AMD64 ABI
  calling conventions, which places the first six 64-bit (or smaller)
  values in registers and pushes the rest onto the stack.  Note that,
  since all LLVM IR operands are 64-bit values, the first six
  operands will always be placed in registers.  (See the notes about
  compiling fdecl below.)

  [ NOTE: It is the caller's responsibility to clean up arguments
  pushed onto the stack, so you must free the stack space after the
  call returns. ]

  [ NOTE: Don't forget to preserve caller-save registers (only if
  needed). ]
*)




(* Compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
  a datastructure, following a path of offsets.  It computes the
  address based on the size of the data, which is dictated by the
  data's type.

  To compile getelmentptr, you must generate x86 code that performs
  the appropriate arithemetic calculations. *)

(* [size_ty] maps an LLVMlite type to a size in bytes.
  (needed for getelementptr)
  - the size of a struct is the sum of the sizes of each component
  - the size of an array of t's with n elements is n * the size of t
  - all pointers, I1, and I64 are 8 bytes
  - the size of a named type is the size of its definition
  - Void, i8, and functions have undefined sizes according to LLVMlite.
    Your function should simply return 0 in those cases *)
let rec size_ty tdecls t : int =
  match t with
  | Void | I8 | Fun _ -> 0
  | I1 | I64 | Ptr _  -> 8
  | Struct s          -> List.fold_left (fun acc x -> acc + (size_ty tdecls x)) 0 s
  | Array (n, ty)     -> n * size_ty tdecls ty
  | Namedt (tid)      -> size_ty tdecls (List.assoc tid tdecls)

(* Generates code that computes a pointer value.

  1. op must be of pointer type: t*

  2. the value of op is the base address of the calculation

  3. the first index in the path is treated as the index into an array
    of elements of type t located at the base address

  4. subsequent indices are interpreted according to the type t:
    - if t is a struct, the index must be a constant n and it
      picks out the n'th element of the struct. [ NOTE: the offset
      within the struct of the n'th element is determined by the
      sizes of the types of the previous elements ]
    - if t is an array, the index can be any operand, and its
      value determines the offset within the array.
    - if t is any other type, the path is invalid

  5. if the index is valid, the remainder of the path is computed as
    in (4), but relative to the type f the sub-element picked out
    by the path so far *)
let compile_gep ctxt (op : Ll.ty * Ll.operand) (path: Ll.operand list) : ins list =
  failwith "compile_gep not implemented"

(* Compiling instructions  -------------------------------------------------- *)

(* This helper function computes the location of the nth incoming
  function argument: either in a register or relative to %rbp,
  according to the calling conventions.  You might find it useful for
  compile_fdecl.

  [ NOTE: the first six arguments are numbered 0 .. 5 ] *)
let arg_loc (n:int) : operand =
  match n with
  | 0 -> Reg Rdi
  | 1 -> Reg Rsi
  | 2 -> Reg Rdx
  | 3 -> Reg Rcx
  | 4 -> Reg R08
  | 5 -> Reg R09
  (* Puts the rest on the stack *)
  | _ -> Ind3 (Lit (Int64.of_int (8 * (n-4))), Rbp)

let compile_bop : Ll.bop -> X86.opcode = function
  | Add  -> Addq
  | Sub  -> Subq
  | Mul  -> Imulq
  | Shl  -> Shlq
  | Lshr -> Shrq
  | Ashr -> Sarq
  | And  -> Andq
  | Or   -> Orq
  | Xor  -> Xorq

let get_uid : Ll.operand -> uid = function
  | Id uid -> uid
  | _      -> failwith "expected uid"

(* The result of compiling a single LLVM instruction might be many x86
  instructions.  We have not determined the structure of this code
  for you. Some of the instructions require only a couple assembly
  instructions, while others require more.  We have suggested that
  you need at least compile_operand, compile_call, and compile_gep
  helpers; you may introduce more as you see fit.

  Here are a few notes:
  - Icmp:  the Set instruction may be of use.  Depending on how you
    compile Cbr, you may want to ensure that the value produced by
    Icmp is exactly 0 or 1.
  - Load & Store: these need to dereference the pointers. Const and
    Null operands aren't valid pointers.  Don't forget to
    Platform.mangle the global identifier.
  - Alloca: needs to return a pointer into the stack
  - Bitcast: does nothing interesting at the assembly level *)
let compile_insn ctxt ((uid:uid), (i:Ll.insn)) : X86.ins list =
  let open X86.Asm in
  match i with
  | Binop (binop, _, op1, op2) ->
    let binopcode = compile_bop binop in
    let reg_op2 = match binopcode with
      | Shlq | Shrq | Sarq -> Rcx
      | _ -> R11 in
    [
      (compile_operand ctxt (Reg R10) op1);
      (compile_operand ctxt (Reg reg_op2) op2);
      (binopcode, [~%reg_op2; ~%R10]);
      (Movq,      [~%R10; lookup ctxt.layout uid]);
    ]
  | Icmp (cnd, _, op1, op2) -> [
      (compile_operand ctxt (Reg R10) op1);
      (compile_operand ctxt (Reg R11) op2);
      (Movq, [~$0; ~%R12]);
      (Cmpq, [~%R11; ~%R10]);
      (Set (compile_cnd cnd), [~%R12]);
      (Movq, [~%R12; lookup ctxt.layout uid]);
    ]
  | Load (_, op) ->
    [compile_operand ctxt (Reg R10) op] @
    (match op with
    | Const _ | Null -> failwith "Load: Invalid pointer"
    | Gid g -> [
        (Movq, [~%R10; lookup ctxt.layout uid])
      ]
    | Id u  -> [
        (Movq, [Ind2 R10; ~%R11]);
        (Movq, [~%R11; lookup ctxt.layout uid])
      ])
  | Store (_, op1, op2) ->
    [compile_operand ctxt (Reg R10) op1] @
    (match op2 with
    | Const _ | Null -> failwith "Store: Invalid pointer"
    | Gid g -> failwith "Store: Gid store unimplemented"
    | Id u  ->
      [compile_operand ctxt (Reg R11) op2] @ [
        (Movq, [~%R10; Ind2 R11])
      ])
  | Alloca (typ) ->
    let x_op1 = lookup ctxt.layout uid in
    let (i, x_op2) = (match x_op1 with
      | Ind3 ((Lit i'), r) -> (Int64.sub i' 8L, Ind3 ((Lit (Int64.sub i' 8L)), r))
      | _ -> failwith "Alloca: Something went wrong") in
    [
      (Movq, [~$0; x_op2]); (* Initialized pointed-to value with 0 *)
      (Movq, [~$(Int64.to_int i); ~%R10]);
      (Addq, [~%Rbp; ~%R10]);
      (Movq, [~%R10; x_op1]) (* Returns an absolute address to the pointer *)
    ]
  | Bitcast (typ1, op, typ2) -> [ (* I think incomplete but it works for the basic level *)
      (compile_operand ctxt (Reg R10) op);
      (Movq, [~%R10; lookup ctxt.layout uid])
    ]
    (* Ind3 (Lit (Int64.of_int (8 * (n-6))), Rsp) *)
  | Call (typ, op, lst) ->
    let rec set_params (lst:(Ll.ty * Ll.operand) list) (n:int) =
      match lst with
      | [] -> []
      | (t,op)::tl -> (compile_operand ctxt (arg_loc n) op) :: (set_params tl (n+1))
      (*
      if (n = last) then []
      else (Movq, [Ind3 (Lit (Int64.of_int (-8 * (n+2))), Rbp); arg_loc (n-1)]) :: set_params (n+1) last
      *)
    in
    let get_label = function
    | Gid lbl -> lbl
    | _ -> failwith "unsupported label"
    in
    (*
    let reg_lst = [
      Rax; Rdx; Rcx; Rsi; Rdi; R08; R09; R10; R11
    ] in
    let push_lst =
      List.map (fun r -> (Pushq, [~%r])) reg_lst
    in
    let pop_lst =
      List.map (fun r -> (Popq, [~%r])) (List.rev reg_lst)
    in
    *)
    [
      (Subq, [~$64; ~%Rsp]);
      (Pushq, [~%Rax]);

      (Pushq, [~%Rdx]);
      (Pushq, [~%Rcx]);
      (Pushq, [~%Rsi]);
      (Pushq, [~%Rdi]);
      (Pushq, [~%R08]);
      (Pushq, [~%R09]);
      (Pushq, [~%R10]);
      (Pushq, [~%R11]);

      (* (Movq, [~$(List.length lst); ~%R09]); *)
    ] @
    [
      (Pushq, [~%Rbp]);
      (Movq, [~%Rsp; ~%Rbp]);
    ] @ (set_params lst 0) @
    [
      (Callq, [Imm (Lbl (Platform.mangle (get_label op)))]);
      (Popq, [~%Rbp]);

      (Popq, [~%R11]);
      (Popq, [~%R10]);
      (Popq, [~%R09]);
      (Popq, [~%R08]);
      (Popq, [~%Rdi]);
      (Popq, [~%Rsi]);
      (Popq, [~%Rcx]);
      (Popq, [~%Rdx]);

      (Movq, [~%Rax; lookup ctxt.layout uid]);
      (Popq, [~%Rax]);
      (Addq, [~$64; ~%Rsp]);
    ]
  | Gep (typ, op, oplst) -> failwith "Gep unimplemented"

let check_call ((uid:uid), (i:Ll.insn)) : string list =
  let open X86.Asm in
  match i with
  | Call (typ, op, lst) ->
    let get_label = function
    | Gid lbl -> lbl
    | Id lbl -> lbl
    | _ -> failwith "unsupported label"
    in [get_label op]
  | _ -> []

(* Debug function *)
let rec print_layout : layout -> unit = function
  | (uid, op)::tl ->
    print_string (uid ^ ", " ^ (string_of_operand op) ^ "\n");
    print_layout tl
  | [] -> print_string "end of layout.\n"

let rec print_strings (lst: string list) =
  match lst with
  | h::tl -> print_string h; print_strings tl
  | [] -> print_string "\n~~~~~~~~~~~~~~~~~BLOCK END~~~~~~~~~~~~~.\n"

(* Compiling terminators  --------------------------------------------------- *)

(* Compile block terminators is not too difficult:
  - Ret should properly exit the function: freeing stack space,
    restoring the value of %rbp, and putting the return value (if
    any) in %rax.
  - Br should jump
  - Cbr branch should treat its operand as a boolean conditional *)
(* type terminator =
   | Ret of ty * operand option
   | Br of lbl
   | Cbr of operand * lbl * lbl *)
let compile_terminator (ctxt:ctxt) (t: (Ll.uid * Ll.terminator)) : X86.ins list =
  let open X86.Asm in
  let get_op r = function (* Move the operand option into Reg r where applicable *)
  | Some op -> compile_operand ctxt (Reg r) op
  | None    -> compile_operand ctxt (Reg r) Null in
  match snd t with
  | Ret (ty', opoption) -> (* Does not free stack space or restore %rbp yet *)
    [
      (get_op Rax opoption);
      (Retq, [])
    ]
  | Br lbl -> (* Labels are not yet resolved in the stack layout. May not actually work *)
    [(Jmp, [Imm (Lbl (Platform.mangle lbl))])]
  | Cbr (op, lbl1, lbl2) -> (* Labels are not yet resolved in the stack layout. May not actually work *)
    [
      (compile_operand ctxt (Reg R11) op);
      (Cmpq,  [~$0; ~%R11]);
      (J Neq, [Imm (Lbl (Platform.mangle lbl1))]);
      (Jmp,   [Imm (Lbl (Platform.mangle lbl2))])
    ]


(* Compiling blocks --------------------------------------------------------- *)

(* We have left this helper function here for you to complete. *)
let compile_block (ctxt:ctxt) (blk:Ll.block) : X86.ins list =
  print_string "BLAWK\n";
  print_string (Ll.get_ty_from_term (snd blk.term));
  print_string "\n";
  let open X86.Asm in
  let rec for_instr : (Ll.uid * Ll.insn) list -> X86.ins list = function
    | h::tl -> compile_insn ctxt h @ for_instr tl
    | []    -> []
  (* Initializes default value for Rax *)
  in (for_instr blk.insns) @ compile_terminator ctxt blk.term

let compile_lbl_block lbl ctxt blk : elem =
  Asm.text lbl (compile_block ctxt blk)

let compile_fun_block (ctxt:ctxt) (blk:Ll.block) : X86.ins list =
  print_string ("\nMade it to fun_block");
  let split_ret (lst:X86.ins list) : (X86.ins list * X86.ins) =
    print_string "\nHello split_ret\n";
    let lst' = List.rev lst in
    match lst' with
    | [] -> failwith "Empty block"
    | h::tl -> (List.rev tl, h)
  in
  print_string "Before split\n";
  let (lst,ret) = split_ret (compile_block ctxt blk) in
  print_string "After split\n";
  let open X86.Asm in
  [
    (*
    (Pushq, [~%R12]);
    (Pushq, [~%R13]);
    (Pushq, [~%R14]);
    (Pushq, [~%R15]);
    *)
  ]
  @ lst
  @
  [
    (*
    (Popq, [~%R15]);
    (Popq, [~%R14]);
    (Popq, [~%R13]);
    (Popq, [~%R12]);
    *)
  ]
  @ [ret]

(* compile_fdecl ------------------------------------------------------------ *)

(* We suggest that you create a helper function that computes the
  stack layout for a given function declaration.
  - each function argument should be copied into a stack slot
  - in this (inefficient) compilation strategy, each local id
    is also stored as a stack slot.
  - see the discusion about locals *)
let stack_layout (args:Ll.uid list) ((block:Ll.block), (lbled_blocks:(lbl*block) list)) : layout =
  let rec add_to_stack (insns:(uid*insn) list) (lbled_blocks:(lbl*block) list) (i:int) : layout =
    match insns with
    | (id, (Alloca typ))::tl -> let n = (size_ty [] typ) / 8 in
      (id, Ind3 (Lit (Int64.of_int (-8 * i)), Rbp))::
      (id ^ "alloca_", Ind3 (Lit (Int64.of_int (-8 * (i + 1))), Rbp))::add_to_stack tl lbled_blocks (i+1+n)
    | (id, instr)::tl -> (id, Ind3 (Lit (Int64.of_int (-8 * i)), Rbp))::add_to_stack tl lbled_blocks (i+1)
    | _ -> match lbled_blocks with
      | (_,blk)::blk_tl -> add_to_stack blk.insns blk_tl i
      | _ -> []
  in
  let rec add_args (lst:Ll.uid list) (n:int) =
    match lst with
    | h::tl -> (h, arg_loc n) :: add_args tl (n+1)
    | [] -> []
  in
  (add_to_stack block.insns lbled_blocks 1) @ (add_args args 0)

(* The code for the entry-point of a function must do several things:
  - since our simple compiler maps local %uids to stack slots,
    compiling the control-flow-graph body of an fdecl requires us to
    compute the layout (see the discussion of locals and layout)

  - the function code should also comply with the calling
    conventions, typically by moving arguments out of the parameter
    registers (or stack slots) into local storage space.  For our
    simple compilation strategy, that local storage space should be
    in the stack. (So the function parameters can also be accounted
    for in the layout.)

  - the function entry code should allocate the stack storage needed
    to hold all of the local stack slots. *)
let compile_fdecl (tdecls:(Ll.tid * Ll.ty) list) (name:string) {f_ty; f_param; f_cfg} : X86.prog =
  let block_layout = stack_layout f_param f_cfg in
  print_string ("Layout size: " ^ (string_of_int (List.length block_layout)));
  let open Asm in
  print_string ("BEGIN: " ^ name ^ "\n");
  let rec compile_fdecl' (tdecls:(Ll.tid * Ll.ty) list) (name:string) {f_ty; f_param; f_cfg} (layout:layout): X86.prog =
    match f_cfg with (blk, blk_lst) ->
    print_string "blk_lst: ";
    print_string (string_of_int (List.length blk_lst));
    let x = [
      if (name = "main") then
      {lbl = name; global = true; asm =
        Text ((Movq, [~$0; ~%Rax]) :: (Movq, [~%Rsp; ~%Rbp]) :: compile_block { tdecls = tdecls; layout = block_layout} blk)
      }
      else
      {lbl = name; global = true; asm =
        Text (compile_fun_block { tdecls = tdecls; layout = block_layout} blk)
      }
    ] in
    print_string "END";
    let y = (match blk_lst with
      | [] -> print_string "what"; []
      | (lbl,blk)::tl -> print_string "neat" ;compile_fdecl' tdecls lbl {f_ty; f_param; f_cfg=(blk, tl)} layout)
    in x @ y in
  print_layout block_layout; compile_fdecl' tdecls name {f_ty; f_param; f_cfg} block_layout

(* compile_gdecl ------------------------------------------------------------ *)
(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label. *)
let rec compile_ginit = function
  | GNull     -> [Quad (Lit 0L)]
  | GGid gid  -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c    -> [Quad (Lit c)]
  | GString s -> [Asciz s]
  | GArray gs | GStruct gs -> List.map compile_gdecl gs |> List.flatten
and compile_gdecl (_, g) = compile_ginit g

(* compile_prog ------------------------------------------------------------- *)
let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl)  -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
