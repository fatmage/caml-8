open Inttypes
open State

type c8_instruction =
  | NoArg of (c8_state -> c8_state)
  | Reg   of (c8_state ->  uint8 ->  c8_state)
  | Addr  of (c8_state ->  uint16 ->  c8_state)
  | DReg  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | RegV  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | DRegV of (c8_state ->  uint8 ->  uint8 ->  uint8 ->  c8_state)


let iNOOP    = NoArg  (fun s -> tick_pc s)

(* 0nnn *)
let iSYS     = Addr   (fun s a -> set_pc s a)                                                             

(* 00E0 *) 
let iCLS     = NoArg  (fun s -> tick_pc (clear_disp s))                                    

(* 00EE *)
let iRET     = NoArg  (fun s -> pop_stack (set_pc s (hd_stack s)))                         

(* 1nnn *)
let iJP      = Addr   (fun s a -> set_pc s a)                                              

(* 2nnn *)
let iCALL    = Addr   (fun s a -> set_pc (push_stack s (U16.add (get_pc s) U16.two)) a)    

(* 3xkk *)
let iSE_rb   = RegV   (fun s reg v -> if U8.eq (get_reg s reg) v then 
                                        tick_pc (tick_pc s) else 
                                        tick_pc s)                                         

(* 4xkk *)
let iSNE_rb  = RegV   (fun s reg v -> if U8.neq (get_reg s reg) v then 
                                      tick_pc (tick_pc s) else 
                                      tick_pc s)                                          

(* 5xy0 *)
let iSE_rr   = DReg   (fun s r1 r2 -> if (U8.eq (get_reg s r1) (get_reg s r2)) then 
                                      tick_pc (tick_pc s) else 
                                      tick_pc s)                                           

(* 6xkk *)
let iLD_rb   = RegV   (fun s reg v -> tick_pc (set_reg s reg v))                          

(* 7xkk *)
let iADD_rb  = RegV   (fun s reg v -> tick_pc (add_reg s reg v))                           

(* 8xy0 *)
let iLD_rr   = DReg   (fun s r1 r2 -> tick_pc (set_reg s r1 (get_reg s r2)))               

(* 8xy1 *)
let iOR_rr   = DReg   (fun s r1 r2 -> tick_pc (set_reg (set_flag s U8.zero) r1 
                                                       (U8.logor 
                                                        (get_reg s r1) (get_reg s r2))))   

(* 8xy2 *)
let iAND_rr  = DReg   (fun s r1 r2 -> tick_pc (set_reg (set_flag s U8.zero) r1 
                                                       (U8.logand 
                                                        (get_reg s r1) (get_reg s r2))))   

(* 8xy3 *)
let iXOR_rr  = DReg   (fun s r1 r2 -> tick_pc (set_reg (set_flag s U8.zero) r1 
                                                       (U8.logxor 
                                                        (get_reg s r1) (get_reg s r2))))   

(* 8xy4 *)
let iADD_rr  = DReg   (fun s r1 r2 -> tick_pc (let res_int = (U8.to_int (get_reg s r1)) + (U8.to_int (get_reg s r2)) in 
                                                let sn = add_reg s r1 (get_reg s r2) in 
                                                  set_flag sn (if res_int > U8.to_int U8.max_val then 
                                                                U8.one else 
                                                                U8.zero)))                 

(* 8xy5 *)
let iSUB_rr  = DReg   (fun s r1 r2 -> tick_pc (let flag_res = if U8.gte (get_reg s r1) (get_reg s r2) then 
                                                              U8.one else U8.zero in 
                                                              set_flag (sub_reg s r1 (get_reg s r2)) flag_res))     

(* 8xy6 *)
let iSHR_rr  = DReg   (fun s r1 r2 ->  tick_pc (let flag_res = U8.logand U8.one (get_reg s r2) in 
                                                set_flag (set_reg s r1 (U8.shr (get_reg s r2) U8.one)) flag_res))   

(* 8xy7 *)
let iSUBN_rr = DReg   (fun s r1 r2 -> tick_pc (let flag_res = if U8.lte (get_reg s r1) (get_reg s r2) then
                                                                U8.one else U8.zero in 
                                                                set_flag (set_reg s r1 (U8.sub (get_reg s r2) (get_reg s r1)))
                                                                         flag_res))                                 

(* 8xyE *)
let iSHL_rr  = DReg   (fun s r1 r2 -> tick_pc (let flag_res = U8.logand U8.one (U8.shr (get_reg s r2) (U8.of_int 7)) in 
                                                              set_flag (set_reg s r1 (U8.shl (get_reg s r2) U8.one)) 
                                                                       flag_res))                                    

(* 9xy0 *)
let iSNE_rr  = DReg   (fun s r1 r2 -> tick_pc (if U8.neq (get_reg s r1) (get_reg s r2) then 
                                                tick_pc s else 
                                                s))                                        

(* Annn *)
let iLD_i    = Addr   (fun s a -> tick_pc (set_ir s a))                                    

(* Bnnn *)
let iJP_0    = Addr   (fun s a -> set_pc s (U16.add (u8_to_16 (get_reg s (U8.zero))) a))   

(* Cxkk *)
let iRND     = RegV   (fun s reg v -> tick_pc (set_reg s reg (U8.logand v (U8.of_int (Random.int 256)))))            

(* Dxyn *)
let iDRW     = DRegV  (fun s r1 r2 v -> tick_pc (draw_sprite s (get_ir s) (U8.rem (get_reg s r2) (U8.of_int 32)) 
                                                                          (U8.rem (get_reg s r1) (U8.of_int 64)) v)) 

(* Ex9E *)
let iSKP     = Reg    (fun s reg -> match check_key s (get_reg s reg) with
                                      | Pressed -> tick_pc (tick_pc s)
                                      | NotPressed -> tick_pc s)                                                     

(* ExA1 *)
let iSKNP    = Reg    (fun s reg -> match check_key s (get_reg s reg) with
                                      | NotPressed -> tick_pc (tick_pc s)
                                      | Pressed -> tick_pc s)                                                        

(* Fx07 *)
let iLD_dtr  = Reg    (fun s reg ->  tick_pc (set_reg s reg (get_dt s)))                                             

(* Fx0A *)
let iLD_key  = Reg    (fun s reg ->  match U8.to_int (find_pressed s) with 
                                      | 0x10 -> s
                                      | x -> tick_pc (set_reg s reg (U8.of_int x)))                                 

(* Fx15 *)
let iLD_rdt  = Reg    (fun s reg -> tick_pc (set_dt s (get_reg s reg)))                                              

(* Fx18 *)
let iLD_str  = Reg    (fun s reg -> tick_pc (set_st s (get_reg s reg)))                                              

(* Fx1E *)
let iADD_i   = Reg    (fun s reg -> tick_pc (add_ir s (u8_to_16 (get_reg s reg))))                                   

(* Fx29 *)
let iLD_font = Reg    (fun s reg -> tick_pc (set_ir s (U16.mul (u8_to_16 (get_reg s reg)) (U16.of_int 5))))          

(* Fx33 *)
let iLD_mem  = Reg    (fun s reg -> let value = get_reg s reg in 
                                      let ten = U8.of_int 10 in
                                        let units = U8.rem value ten in 
                                          let tens = U8.rem (U8.div value ten) ten in 
                                            let hund = U8.rem (U8.div value (U8.of_int 100)) ten in
                                              let addr = get_ir s in
                                                tick_pc (update_mem 
                                                          (update_mem 
                                                            (update_mem s (U16.succ (U16.succ addr)) units) 
                                                          (U16.succ addr) tens) 
                                                         addr hund))                                                 

(* Fx55 *)
let iLD_memi = Reg    (fun s reg -> let rec helper = fun s ir curr max ->
                                      if U8.lt curr max then
                                        helper (update_mem s ir (get_reg s curr)) (U16.succ ir) (U8.succ curr) max
                                      else 
                                        update_mem s ir (get_reg s curr) in
                                        tick_pc (add_ir (helper s (get_ir s) U8.zero reg) U16.one))                  

(* Fx65 *) 
let iLD_regi = Reg    (fun s reg ->  let rec helper = fun s ir curr max ->
                                      if U8.lte curr max then
                                        helper (set_reg s curr (fetch_mem s ir)) (U16.succ ir) (U8.succ curr) max 
                                      else
                                        s in 
                                        tick_pc (add_ir (helper s (get_ir s) U8.zero reg) U16.one))                 


let decode_opcode : uint16 -> c8_instruction = 
  fun opcode -> let opcode_int = U16.to_int opcode in 
                  match opcode_int with 
                    | 0x00E0 -> iCLS
                    | 0x00EE -> iRET
                    | x ->  let opcode_int = U16.to_int (U16.logand opcode (U16.of_int 0xF000)) in 
                              match opcode_int with
                                | 0x0000 -> iSYS
                                | 0x1000 -> iJP
                                | 0x2000 -> iCALL
                                | 0x3000 -> iSE_rb
                                | 0x4000 -> iSNE_rb
                                | 0x5000 -> iSE_rr
                                | 0x6000 -> iLD_rb
                                | 0x7000 -> iADD_rb
                                | 0x8000 -> let opcode_int = U16.to_int (U16.logand opcode (U16.of_int 0x000F)) in 
                                              begin match opcode_int with 
                                                | 0x0000 -> iLD_rr
                                                | 0x0001 -> iOR_rr
                                                | 0x0002 -> iAND_rr
                                                | 0x0003 -> iXOR_rr
                                                | 0x0004 -> iADD_rr
                                                | 0x0005 -> iSUB_rr
                                                | 0x0006 -> iSHR_rr
                                                | 0x0007 -> iSUBN_rr
                                                | 0x000E -> iSHL_rr
                                                | _ -> iNOOP
                                            end
                                | 0x9000 -> iSNE_rr
                                | 0xA000 -> iLD_i
                                | 0xB000 -> iJP_0
                                | 0xC000 -> iRND
                                | 0xD000 -> iDRW
                                | 0xE000 -> let opcode_int = U16.to_int (U16.logand opcode (U16.of_int 0x00FF)) in 
                                              begin match opcode_int with 
                                                | 0x009E -> iSKP
                                                | 0x00A1 -> iSKNP
                                                | _ -> iNOOP
                                              end
                                | 0xF000 -> let opcode_int = U16.to_int (U16.logand opcode (U16.of_int 0x00FF)) in 
                                              begin match opcode_int with 
                                                | 0x0007 -> iLD_dtr
                                                | 0x000A -> iLD_key
                                                | 0x0015 -> iLD_rdt 
                                                | 0x0018 -> iLD_str 
                                                | 0x001E -> iADD_i 
                                                | 0x0029 -> iLD_font 
                                                | 0x0033 -> iLD_mem 
                                                | 0x0055 -> iLD_memi 
                                                | 0x0065 -> iLD_regi
                                                | _ -> iNOOP
                                              end
                                | _ -> iNOOP
