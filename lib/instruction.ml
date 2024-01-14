open Memory
open State
open Inttypes


type instruction =
  | NoArg of (c8_state -> c8_state)
  | Reg   of (c8_state ->  uint8 ->  c8_state)
  | Addr  of (c8_state ->  uint16 ->  c8_state)
  | DReg  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | RegV  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | DRegV of (c8_state ->  uint8 ->  uint8 ->  uint8 ->  c8_state)


let iNOOP    = NoArg  (fun s -> s)
let iSYS     = Addr   (fun s a -> set_pc s a)                                                       (* 0nnn *)               
let iCLS     = NoArg  (fun s -> s)                                                                 (* 00E0 *) 
let iRET     = NoArg  (fun s -> pop_stack (set_pc s (hd_stack s)))                                  (* 00EE *)
let iJP      = Addr   (fun s a -> set_pc s a)                                                          (* 1nnn *)
let iCALL    = Addr   (fun s a -> set_pc (push_stack s (U16.add (get_pc s) U16.two)) a)               (* 2nnn *)
let iSE_rb   = RegV   (fun s reg v -> if U8.eq (get_reg s reg) v then tick_pc s else s)             (* 3xkk *)
let iSNE_rb  = RegV   (fun s reg v -> if U8.neq (get_reg s reg) v then tick_pc s else s)            (* 4xkk *)
let iSE_rr   = DReg   (fun s r1 r2 -> if r1 == r2 then tick_pc s else s)                          (* 5xy0 *)
let iLD_rb   = RegV   (fun s reg v -> set_reg s reg v)                                          (* 6xkk *)
let iADD_rb  = RegV   (fun s reg v -> set_reg s reg (U8.add (get_reg s reg) v))                       (* 7xkk *)
let iLD_rr   = DReg   (fun s r1 r2 -> set_reg s r1 (get_reg s r2))                                            (* 8xy0 *)
let iOR_rr   = DReg   (fun s r1 r2 -> set_reg (set_flag s U8.zero) r1 (U8.logor (get_reg s r1) (get_reg s r2)))  (* 8xy1 *)
let iAND_rr  = DReg   (fun s r1 r2 -> set_reg (set_flag s U8.zero) r1 (U8.logand (get_reg s r1) (get_reg s r2)))  (* 8xy2 *)
let iXOR_rr  = DReg   (fun s r1 r2 -> set_reg (set_flag s U8.zero) r1 (U8.logxor (get_reg s r1) (get_reg s r2)))  (* 8xy3 *)
let iADD_rr  = DReg   (fun s r1 r2 -> let res_int = (U8.to_int (get_reg s r1)) + (U8.to_int (get_reg s r2)) in 
                                        let res_reg = U8.add (get_reg s r1) (get_reg s r2) in  
                                          let sn = set_reg s r1 res_reg in 
                                            set_flag sn (if res_int > U8.to_int U8.max_val then U8.one else U8.zero))       (* 8xy4 *)
let iSUB_rr  = DReg   (fun s r1 r2 -> let flag_res = if U8.gte (get_reg s r1) (get_reg s r2) then U8.one else U8.zero in 
                                        set_reg (set_flag s flag_res) r1 (U8.sub (get_reg s r1) (get_reg s r2)))            (* 8xy5 *)
let iSHR_rr  = DReg   (fun s r1 r2 -> let flag_res = U8.logand U8.one (get_reg s r2) in 
                                        set_reg (set_flag s flag_res) r1 (U8.shr (get_reg s r2) U8.one))                     (* 8xy6 *)
let iSUBN_rr = DReg   (fun s r1 r2 -> let flag_res = if U8.lte (get_reg s r1) (get_reg s r2) then U8.one else U8.zero in 
                                        set_reg (set_flag s flag_res) r1 (U8.sub (get_reg s r1) (get_reg s r2)))               (* 8xy7 *)
let iSHL_rr  = DReg   (fun s r1 r2 -> let flag_res = U8.logand U8.one (U8.shr (get_reg s r2) (U8.of_int 7)) in 
                                        set_reg (set_flag s flag_res) r1 (U8.shr (get_reg s r2) U8.one))                   (* 8xyE *)
let iSNE_rr  = DReg   (fun s r1 r2 -> if U8.neq (get_reg s r1) (get_reg s r2) then tick_pc s else s)                     (* 9xy0 *)
let iLD_i    = Addr   (fun s a -> s)                            (* Annn *)
let iJP_0    = Addr   (fun s a -> s)   (* Bnnn *)
let iRND     = RegV   (fun s reg v -> s)  (* Cxkk *)
let iDRW     = DRegV  (fun s r1 r2 v -> s)    (* Dxyn *)
let iSKP     = Reg    (fun s reg -> s)     (* Ex9E *)
let iSKNP    = Reg    (fun s reg -> s)    (* ExA1 *)
let iLD_dtr  = Reg    (fun s reg -> s)  (* Fx07 *)
let iLD_key  = Reg    (fun s reg -> s)  (* Fx0A *)
let iLD_rdt  = Reg    (fun s reg -> s)  (* Fx15 *)
let iLD_str  = Reg    (fun s reg -> s)  (* Fx18 *)
let iADD_i   = Reg    (fun s reg -> s)   (* Fx1E *)
let iLD_font = Reg    (fun s reg -> s) (* Fx29 *)
let iLD_mem  = Reg    (fun s reg -> s)  (* Fx33 *)
let iLD_memi = Reg    (fun s reg -> s) (* Fx55 *)
let iLD_regi = Reg    (fun s reg -> s) (* Fx65 *) 


let opcode_to_instruction : uint16 -> instruction = fun _ -> iNOOP

let fetch_instruction = fun mem -> fun addr -> opcode_to_instruction (fetch_opcode mem addr)