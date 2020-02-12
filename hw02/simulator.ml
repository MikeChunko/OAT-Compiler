(* X86lite Simulator *)
(* Author: Michael Chunko, Dominick DiMaggio                                  *)
(* Pledge: I pledge my honor that I have abided by the Stevens Honor System.  *)
open X86

(* Simulator machine state *)
let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* The simulator will raise this exception if it tries to read from or
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
       0x400010 :  InsFrag *)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 7th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* Memory maps addresses to symbolic bytes *)
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

(* Simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

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
    | _ -> 0L in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (Pervasives.pred i) in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) ->
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> () in
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
let interp_cnd {fo; fs; fz} : cnd -> bool = function
  | Eq  -> fz
  | Neq -> not fz
  | Gt  -> (fs = fo) && not fz
  | Ge  -> fs = fo
  | Lt  -> fs <> fo
  | Le  -> (fs <> fo) || fz

(* Set the machine flags based on the given value(s). *)
let set_cnd (m:mach) (fo:bool) (fs:bool) (fz:bool) : unit =
  m.flags.fo <- fo; m.flags.fs <- fs; m.flags.fz <- fz

let set_cnd_oflow (m:mach) ({value=v; overflow=o}:Int64_overflow.t) : unit =
  set_cnd m o (v < 0L) (v = 0L)

let set_cnd_logic (m:mach) (v:int64) : unit =
  set_cnd m false (v < 0L) (v = 0L)

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if addr < mem_bot || addr >= mem_top then None
  else Some (Int64.to_int (Int64.sub addr mem_bot))

let get_addr (addr:quad) : int =
  match (map_addr addr) with
  | Some v -> v
  | None   -> invalid_arg "Invalid address"

(* Set the int64 value stored in an operator (where applicable).
   If the operand cannot store a value, fail. *)
let store_value (m:mach) (v:int64) : operand -> unit =
  let store (m:mach) (addr:quad) (v:int64) : unit =
    let open Array in
    blit (of_list (sbytes_of_int64 v)) 0 m.mem (get_addr addr) 8 in function
  | Reg r           -> m.regs.(rind r) <- v
  | Imm i           -> failwith "Cannot store to immediate"
  | Ind1 (Lit i)    -> store m i v
  | Ind2 (r)        -> store m (m.regs.(rind r)) v
  | Ind3 (Lit i, r) -> store m (Int64.add i m.regs.(rind r)) v
  | _               -> invalid_arg "Cannot store to label"

(* Get the instruction at %rip and increment %rip by 4. *)
let get_rip (m:mach) : ins =
  match (map_addr m.regs.(rind Rip)) with
  | None   -> raise X86lite_segfault (* Invalid address. *)
  | Some x ->
    match m.mem.(x) with
    | InsB0 i -> store_value m (Int64.add m.regs.(rind Rip) 8L) (Reg Rip); i
    | _       -> raise X86lite_segfault (* Invalid instruction. *)

(* Get the int64 value/address of an operand. *)
let get_value (m:mach) : operand -> int64 =
  let get (m:mach) (v:int) : int64 =
    let open Array in
    int64_of_sbytes (to_list (sub m.mem v 8))
  in function
  | Imm (Lit i)     -> i
  | Reg r           -> m.regs.(rind r)
  | Ind1 (Lit i)    -> get m (get_addr i)
  | Ind2 r          -> get m (get_addr m.regs.(rind r))
  | Ind3 (Lit i, r) -> get m ((Int64.to_int i) + (get_addr m.regs.(rind r)))
  | _               -> invalid_arg "Cannot get value of a label"

(* Wraps get_value and returns an int for bit shifting *)
let get_shamt (m:mach) (o:operand) : int =
  Int64.to_int (get_value m o)

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags *)
let step (m:mach) : unit =
  let open Int64 in
  let open Int64_overflow in
  (* Take in a function and register(s), perform the function based on register value(s),
     store in the proper register, and update condition flags.  *)
  let unary f d = let oflow = (f (get_value m d)) in
    set_cnd_oflow m oflow; store_value m oflow.value d in
  let logic_unary f d = let value = (f (get_value m d)) in
    set_cnd_logic m value; store_value m value d in
  let binary f s d = let oflow = (f (get_value m d) (get_value m s)) in
    set_cnd_oflow m oflow; store_value m oflow.value d in
  let logic_binary f s d = let value = (f (get_value m d) (get_value m s)) in
    set_cnd_logic m value; store_value m value d in
  (* Helper functions for specific instructions. *)
  let shift f s d opc =
    let shamt = get_shamt m s in
    let before = get_value m d in
    let after = f before shamt in
    if shamt <> 0 then ( (* Flags unaffected if no shift is done *)
      m.flags.fs <- after < 0L; m.flags.fz <- after = 0L;
      store_value m after d;
      match opc with
      | Sarq -> m.flags.fo <- shamt = 1
      | Shlq -> m.flags.fo <- (logand (shift_left 1L 63) before) = (logand (shift_left 1L 62) before)
      | Shrq -> m.flags.fo <- (logand (shift_left 1L 63) before) = 1L
      | _ -> invalid_arg "Incorrect operation passed") in
  let replace_low_byte (i: int64) (b: int64) : int64 =
    let new_i = (logand (lognot 255L) i) in
    let new_b = (logand 255L b) in
    logor new_i new_b in
  let push s = (* Decrease stack by 8 bytes and store the new value *)
    m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
    store_value m (get_value m s) (Ind2 Rsp) in
  let pop d = (* Return the latest stack value and increase pointer by 8 bytes *)
    store_value m (get_value m (Ind2 Rsp)) d;
    m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L in
  let cmp f s d = let oflow = (f (get_value m d) (get_value m s)) in
    set_cnd_oflow m oflow in
  let jump s = store_value m (get_value m s) (Reg Rip) in
  match (get_rip m) with
  | (Negq,  [d])    -> unary neg d
  | (Addq,  [s; d]) -> binary add s d
  | (Subq,  [s; d]) -> binary sub s d
  | (Imulq, [s; d]) -> binary mul s d
  | (Incq,  [s])    -> unary succ s
  | (Decq,  [s])    -> unary pred s
  | (Notq,  [s])    -> logic_unary lognot s
  | (Andq,  [s; d]) -> logic_binary logand s d
  | (Orq,   [s; d]) -> logic_binary logor s d
  | (Xorq,  [s; d]) -> logic_binary logxor s d
  | (Sarq,  [a; d]) -> shift shift_right a d Sarq
  | (Shlq,  [a; d]) -> shift shift_left a d Shlq
  | (Shrq,  [a; d]) -> shift shift_right_logical a d Shrq
  | (Set c, [d])    -> store_value m (replace_low_byte (get_value m d) (if interp_cnd m.flags c then 1L else 0L)) d
  | (Leaq,  [i; d]) -> store_value m (get_value m i) d
  | (Movq,  [s; d]) -> store_value m (get_value m s) d
  | (Pushq, [s])    -> push s
  | (Popq,  [d])    -> pop d
  | (Cmpq,  [s; d]) -> cmp sub s d
  | (Jmp,   [s])    -> jump s
  | (Callq, [s])    -> push (Reg Rip); jump s
  | (Retq,  [])     -> pop (Reg Rip)
  | (J c,   [s])    -> if interp_cnd m.flags c then jump s
  | _               -> invalid_arg "Unimplemented instruction"

(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 =
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* Assembling and linking --------------------------------------------------- *)

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
   - the data segment starts after the text segment *)
let assemble (p:prog) : exec =
  let open List in
  let open Int64 in
  let data_length = function
    | Asciz s -> String.length s + 1
    | Quad _  -> 8 in
  (* Symbol table representation and helper functions. *)
  let symbol_table : (lbl, quad) Hashtbl.t = Hashtbl.create 100 in
  let symbol_add (label:lbl) (pos:quad) : unit =
    if Hashtbl.mem symbol_table label then raise (Redefined_sym label)
    else Hashtbl.add symbol_table label pos in
  let symbol_lookup (label:lbl) : quad =
    if Hashtbl.mem symbol_table label then Hashtbl.find symbol_table label
    else raise (Undefined_sym label) in
  let symbol_exist (label:lbl) : bool =
    Hashtbl.mem symbol_table label in
  (* "Fix" instructions (Replace labels with their correct values). *)
  let fix_imm : imm -> imm = function
    | Lbl l -> Lit (symbol_lookup l)
    | i     -> i in
  let fix_operand : operand -> operand = function
    | Imm i       -> Imm (fix_imm i)
    | Ind1 i      -> Ind1 (fix_imm i)
    | Ind3 (i, r) -> Ind3(fix_imm i, r)
    | op          -> op in
  let fix_ins (opcode,operand:ins) : ins =
    (opcode, map fix_operand operand) in
  let fix_data : data -> data = function
    | Quad i -> Quad (fix_imm i)
    | d      -> d in
  let fix : asm -> asm = function
    | Text l -> Text (map fix_ins l)
    | Data l -> Data (map fix_data l) in
  let text_pos = ref mem_bot in
  let data_pos = ref mem_bot in
  (* Find the position of sections, add labels to the symbol table. *)
  let elem_init (e:elem) : unit =
    match e.asm with
    | Text l -> symbol_add e.lbl !text_pos;
                text_pos := add !text_pos (mul ins_size (of_int (length l)));
                data_pos := add !data_pos (mul ins_size (of_int (length l)))
    | Data l -> symbol_add e.lbl !text_pos;
                text_pos := add !text_pos (of_int (fold_left (fun a b -> a + data_length b) 0 l)) in
  let start : exec =
    iter elem_init p;
    { entry = if symbol_exist "main" then symbol_lookup "main"
              else raise (Undefined_sym "main")
    ; text_pos = mem_bot
    ; data_pos = !data_pos
    ; text_seg = []
    ; data_seg = []} in
  let assemble_step (a:asm) (e:exec) : exec =
    match (fix a) with
    | Text l -> {e with text_seg = concat (map sbytes_of_ins l) @ e.text_seg}
    | Data l -> {e with data_seg = concat (map sbytes_of_data l) @ e.data_seg} in
  fold_right assemble_step (map (fun e -> e.asm) p) start

(* Convert an object file into an executable machine state.
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the
      appropriate locations
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory
      - the other registers are initialized to 0
    - the condition code flags start as 'false' *)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach =
  let open Array in
  let open Int64 in
  let flags = {fo = false; fs = false; fz = false} in
  let regs = make 17 0L in
  regs.(rind Rip) <- entry;
  regs.(rind Rsp) <- sub mem_top 8L; (* The highest address in mem *)
  let mem = make mem_size (Byte '\x00') in
  let text_arr = of_list text_seg in
  blit text_arr 0 mem (get_addr text_pos) (length text_arr);
  let data_arr = of_list data_seg in
  blit data_arr 0 mem (get_addr data_pos) (length data_arr);
  blit (of_list (sbytes_of_int64 exit_addr)) 0 mem (mem_size - 8) 8;
  {flags; regs; mem}
