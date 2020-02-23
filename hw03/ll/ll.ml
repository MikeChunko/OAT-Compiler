(* LLVMlite: A simplified subset of LLVM IR *)

(* Local identifiers *)
type uid = string

(* Global identifiers *)
type gid = string

(* Named types *)
type tid = string

(* Labels *)
type lbl = string

(* LLVM types *)
type ty =
| Void
| I1
| I8
| I64
| Ptr of ty
| Struct of ty list
| Array of int * ty
| Fun of ty list * ty
| Namedt of tid

let rec string_of_ty (t:ty) : string =
    begin match t with
    | Void -> "Void, "
    | I1 -> "I1, "
    | I8 -> "I8, "
    | I64 -> "I64, "
    | Ptr t1 -> "Ptr of " ^ string_of_ty t1
    | Struct tl -> "Struct of [" ^ print_tylist tl ^ "]"
    | _ -> "Unimplemented print, "
    end
and
print_tylist (lst: ty list) = 
    match lst with
    | h::tl -> string_of_ty h ^ print_tylist tl
    | [] -> ", END"
(* Function type: argument types and return type *)
type fty = ty list * ty



let rec print_strlist (lst: string list) =
    match lst with
    | h::tl -> h ^ ", " ^ print_strlist tl
    | _ -> ", END"

(* Syntactic Values *)
type operand =
| Null
| Const of int64
| Gid of gid
| Id of uid

(* Binary i64 Operations *)
type bop =
| Add
| Sub
| Mul
| Shl
| Lshr
| Ashr
| And
| Or
| Xor

(* Comparison Operators *)
type cnd =
| Eq
| Ne
| Slt
| Sle
| Sgt
| Sge

(* Instructions *)
type insn =
| Binop of bop * ty * operand * operand
| Alloca of ty
| Load of ty * operand
| Store of ty * operand * operand
| Icmp of cnd * ty * operand * operand
| Call of ty * operand * (ty * operand) list
| Bitcast of ty * operand * ty
| Gep of ty * operand * operand list

let rec bop_to_str inp =
    match inp with
    | Add -> "Add"
    | Sub -> "Sub"
    | Mul -> "Mul"
    | Shl -> "Shl"
    | Lshr -> "Lshr"
    | Ashr -> "Ashr"
    | And -> "And"
    | Or -> "Or"
    | Xor -> "Xor"

let rec op_to_str inp =
    match inp with
    | Null -> "Null"
    | Const i -> "Const " ^ Int64.to_string i
    | Gid g -> "Gid " ^ g
    | Id u -> "Uid " ^ u

let rec insn_to_str inp =
    match inp with
    | Binop (bop, ty, operand, operand2) -> "Binop of " ^ (bop_to_str bop) ^ ", " ^ (string_of_ty ty) ^ (op_to_str operand) ^ ", " ^ (op_to_str operand2) ^ "|"
    | Alloca ty -> "Alloca of " ^ (string_of_ty ty) ^ "|"
    | Load (ty, operand) -> "Load from " ^ (string_of_ty ty) ^ (op_to_str operand) ^ "|"
    | Store (ty, operand, operand2) -> "Store to " ^ (string_of_ty ty) ^ ", " ^ (op_to_str operand) ^ ", " ^ (op_to_str operand2)  ^ "|"
    | Icmp (cnd, ty, operand, operand2) -> "Icmp of cnd, " ^ ", " ^ (string_of_ty ty) ^ ", " ^ (op_to_str operand) ^ ", " ^ (op_to_str operand2) ^ "|"
    | Call (ty, operand, (ty2, operand2)::tl) -> "Call"
    | Bitcast (ty, operand, ty2) -> "Bitcast"
    | Gep (ty, operand, operand2::tl) -> "Gep"
    | _ -> "Empty"

(* Returning void doesn't need operand *)
type terminator =
| Ret of ty * operand option
| Br of lbl
| Cbr of operand * lbl * lbl

(* Basic Blocks *)
type block = { insns : (uid * insn) list; term : (uid * terminator) }

let rec block_to_string (blk:(uid * insn) list) =
    match blk with
    | (a,b) :: tl -> (insn_to_str b) ^ ", " ^ (block_to_string tl)
    | [] -> ""
    

(* Control Flow Graphs: entry and labeled blocks *)
type cfg = block * (lbl * block) list

(* Function Declarations *)
type fdecl = { f_ty : fty; f_param : uid list; f_cfg : cfg }

(* Global Data Initializers *)
type ginit =
| GNull
| GGid of gid
| GInt of int64
| GString of string
| GArray of (ty * ginit) list
| GStruct of (ty * ginit) list

(* Global Declarations *)
type gdecl = ty * ginit

(* LLVMlite Programs *)
type prog = { tdecls : (tid * ty) list; gdecls : (gid * gdecl) list;
              fdecls : (gid * fdecl) list; edecls : (gid * ty) list }
