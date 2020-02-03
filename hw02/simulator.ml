(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next seven bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 7th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) ->
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"

(* It might be useful to toggle printing of intermediate states of your
   simulator. *)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun x ->
  match x with
  | Eq  -> fz
  | Neq -> not fz
  | Gt  -> (fs = fo) && not fz
  | Ge  -> fs = fo
  | Lt  -> fs <> fo
  | Le  -> (fs <> fo) || fz

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if addr < mem_bot || addr >= mem_top then None
  else Some (Int64.to_int (Int64.sub addr mem_bot))

(* DEBUG *)
open X86
open Assert
open Asm

let sbyte_list (a : sbyte array) (start: int) : sbyte list =
  Array.to_list (Array.sub a start 8)

let stack_offset (i : quad) : operand = Ind3 (Lit i, Rsp)

let test_machine (bs : sbyte list) : mach =
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list bs) 0 mem 0 (List.length bs);
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- mem_bot;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }

let negq = test_machine
    [InsB0 (Movq, [~$42; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [~$(-24); stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Movq, [Imm (Lit Int64.min_int); ~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [stack_offset 0L]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ;InsB0 (Negq, [~%Rbx]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
    ]

(* END DEBUG *)

(* Get the instruction at %rip and increment %rip by 4. *)
let get_rip (m:mach) : ins =
  let rip = (m.regs.(rind Rip)) in
  match (map_addr rip) with
  | None -> raise X86lite_segfault (* Invalid address. *)
  | Some x ->
    match m.mem.(x) with
    | InsB0 i -> (m.regs.(rind Rip) <- (Int64.add rip 8L)); i
    | _ -> raise X86lite_segfault (* Invalid instruction. *)

(* Get the int64 value of an operand. *)
let get_value (m:mach) (o:operand) : int64 =
  match o with
  | Imm (Lit x) -> x
  | Reg x -> m.regs.(rind x)
  | _ -> raise X86lite_segfault

(* Set the int64 value stored in a register. *)
let set_reg (m:mach) (r:reg) (v:int64) : int64 =
  (m.regs.(rind r) <- v); v

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
(* TODO:
   - Use Int64_overflow to set condition flags
   - Create function to properly set condition flags for logical instructions 
   - Finish matching of other instructions
     - These probably require custom functions instead of unary, binary *)
let step (m:mach) : unit =
  let open Int64_overflow in
  (* Helper functions: Take in a function and register(s),
     performs the function based on register values,
     stores it in the proper register,
     and updates condition flags.  *)
  let unary f d = set_reg m d (f (get_value m d)).value in
  let binary f s d = set_reg m s (f (get_value m s) (get_value m d)).value in
  match (get_rip m) with
  | (Negq,  [d])    -> unary neg d; () (* Temporary *)
  | (Addq,  [s; d]) -> binary add s d; () (* Temporary *)
  | (Subq,  [s; d]) -> binary sub s d; () (* Temporary *)
  | (Imulq, [s; d]) -> binary mul s d; () (* Temporary *)
  | (Incq,  [s])    -> unary succ s; () (* Temporary *)
  | (Decq,  [s])    -> unary pred s; () (* Temporary *)
  | (Notq,  [d])    -> unary Int64.lognot s; () (* Temporary *)
  | (Andq,  [s; d]) -> binary Int64.logand s d; () (* Temporary *)
  | (Orq,   [s; d]) -> binary Int64.logor s d; () (* Temporary *)
  | (Xorq,  [s; d]) -> binary Int64.logxor s d; () (* Temporary *)
  | (Sarq,  [a; d]) -> binary Int64.shift_right d a; () (* Temporary *)
  | (Shlq,  [a; d]) -> binary Int64.shift_left d a; () (* Temporary *)
  | (Shrq,  [a; d]) -> binary Int64.shift_right_logical d a; () (* Temporary *)
  | (Set c, [d])    -> failwith "Setb"
  | (Leaq,  [i; d]) -> failwith "Leaq"
  | (Movq,  [s; d]) -> failwith "Movq"
  | (Pushq, [s])    -> failwith "Pushq"
  | (Popq,  [d])    -> failwith "Popq"
  | (Cmpq,  [s; d]) -> failwith "Cmpq"
  | (Jmp,   [s])    -> failwith "Jmp"
  | (Callq, [s])    -> failwith "Callq"
  | (Retq,  [])     -> failwith "Retq"
  | (J c,   [s])    -> failwith "J"
  | _ -> failwith "Unimplemented instruction"

(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 =
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
  failwith "assemble unimplemented"

(* Convert an object file into an executable machine state.
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the
      appropriate locations
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach =
  failwith "load unimplemented"
