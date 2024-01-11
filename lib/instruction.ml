open Memory
open State



type c8_byte = int
type c8_nibble  
type c8_opcode = int


type instruction =
  | NoArg of  (c8_state -> c8_state)
  | Reg of  (c8_state ->  c8_register ->  c8_state)
  | Addr of  (c8_state ->  c8_address ->  c8_state)
  | DReg of  (c8_state ->  c8_register ->  c8_register ->  c8_state)
  | RegVal of  (c8_state ->  c8_register ->  c8_byte ->  c8_state)
  | DRegVal of  (c8_state ->  c8_register ->  c8_register ->  c8_nibble ->  c8_state)


let iNOOP    = NoArg (fun s -> s)
let iSYS     = Addr  (fun s -> fun a -> set_pc s a) (* 0nnn *)               
let iCLS     = NoArg (fun s -> s)                                    (* 00E0 *)
let iRET     = NoArg (fun s -> s)                                          (* 00EE *)
let iJP      = Addr (fun s -> fun a -> set_pc s a)   (* 1nnn *)
let iCALL    = Addr (fun s -> fun a -> s)  (* 2nnn *)
let iSE_rb   = RegVal (fun s -> fun reg -> fun v -> if reg == v then tick_pc s else s) (* 3xkk *)
let iSNE_rb  = RegVal (fun s -> fun reg -> fun v -> if reg != v then tick_pc s else s) (* 4xkk *)
let iSE_rr   = DReg   (fun s -> fun r1 -> fun r2 -> if r1 == r2 then tick_pc s else s) (* 5xy0 *)
let iLD_rb   = RegVal (fun s -> fun reg -> fun v -> s)  (* 6xkk *)
let iADD_rb  = RegVal (fun s -> fun reg -> fun v -> s) (* 7xkk *)
let iLD_rr   = DReg (fun s -> fun r1 -> fun r2 -> s) (* 8xy0 *)
let iOR_rr   = DReg (fun s -> fun r1 -> fun r2 -> s)  (* 8xy1 *)
let iAND_rr  = DReg (fun s -> fun r1 -> fun r2 -> s)  (* 8xy2 *)
let iXOR_rr  = DReg (fun s -> fun r1 -> fun r2 -> s)  (* 8xy3 *)
let iADD_rr  = DReg (fun s -> fun r1 -> fun r2 -> s)  (* 8xy4 *)
let iSUB_rr  = DReg (fun s -> fun r1 -> fun r2 -> s)  (* 8xy5 *)
let iSHR_rr  = DReg (fun s -> fun r1 -> fun r2 -> s) (* 8xy6 *)
let iLD_i    = Addr (fun s -> fun a -> s)(* Annn *)
let iJP_0    = Addr (fun s -> fun a -> s)   (* Bnnn *)
let iRND     = RegVal (fun s -> fun reg -> fun v -> s)  (* Cxkk *)
let iDRW     = DRegVal (fun s -> fun r1 -> fun r2 -> fun v -> s)    (* Dxyn *)
let iSKP     = Reg (fun s -> fun reg -> s)     (* Ex9E *)
let iSKNP    = Reg (fun s -> fun reg -> s)    (* ExA1 *)
let iLD_dtr  = Reg (fun s -> fun reg -> s)  (* Fx07 *)
let iLD_key  = Reg (fun s -> fun reg -> s)  (* Fx0A *)
let iLD_rdt  = Reg (fun s -> fun reg -> s)  (* Fx15 *)
let iLD_str  = Reg (fun s -> fun reg -> s)  (* Fx18 *)
let iADD_i   = Reg (fun s -> fun reg -> s)   (* Fx1E *)
let iLD_font = Reg (fun s -> fun reg -> s) (* Fx29 *)
let iLD_mem  = Reg (fun s -> fun reg -> s)  (* Fx33 *)
let iLD_memi = Reg (fun s -> fun reg -> s) (* Fx55 *)
let iLD_regi = Reg (fun s -> fun reg -> s) (* Fx65 *) 


let opcode_to_instruction : c8_opcode -> instruction = fun _ -> iNOOP

let fetch_instruction = fun mem -> fun addr -> opcode_to_instruction (fetch_opcode mem addr)