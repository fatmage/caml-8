type c8_address ;;
type c8_register ;;
type c8_byte ;;
type c8_nibble ;;


type instruction =
  | NOOP
  | SYS                                              (* 0nnn *)
  | CLS                                              (* 00E0 *)
  | RET                                              (* 00EE *)
  | JP      of c8_address                            (* 1nnn *)
  | CALL    of c8_address                            (* 2nnn *)
  | SE_rb   of c8_register * c8_byte                 (* 3xkk *)
  | SNE_rb  of c8_register * c8_byte                 (* 4xkk *)
  | SE_rr   of c8_register * c8_register             (* 5xy0 *)
  | LD_rb   of c8_register * c8_byte                 (* 6xkk *)
  | ADD_rb  of c8_register * c8_byte                 (* 7xkk *)
  | LD_rr   of c8_register * c8_register             (* 8xy0 *)
  | OR_rr   of c8_register * c8_register             (* 8xy1 *)
  | AND_rr  of c8_register * c8_register             (* 8xy2 *)
  | XOR_rr  of c8_register * c8_register             (* 8xy3 *)
  | ADD_rr  of c8_register * c8_register             (* 8xy4 *)
  | SUB_rr  of c8_register * c8_register             (* 8xy5 *)
  | SHR_rr  of c8_register * c8_register             (* 8xy6 *)
  | SUBN_rr of c8_register * c8_register             (* 8xy7 *)
  | SHL_rr  of c8_register * c8_register             (* 8xyE *)
  | SNE_rr  of c8_register * c8_register             (* 9xy0 *)
  | LD_i    of c8_address                            (* Annn *)
  | JP_0    of c8_address                            (* Bnnn *)
  | RND     of c8_register * c8_byte                 (* Cxkk *)
  | DRW     of c8_register * c8_register * c8_nibble (* Dxyn *)
  | SKP     of c8_register                           (* Ex9E *)
  | SKNP    of c8_register (*                           ExA1 *)
  | LD_dtr  of c8_register                           (* Fx07 *)
  | LD_key  of c8_register (*                           Fx0A *)
  | LD_rdt  of c8_register                           (* Fx15 *)
  | LD_str  of c8_register (*                           Fx18 *)
  | ADD_i   of c8_register                           (* Fx1E *)
  | LD_font of c8_register (*                           Fx29 *)
  | LD_mem  of c8_register                           (* Fx33 *)
  | LD_memi of c8_register (*                           Fx55 *)
  | LD_regi of c8_register ;;                        (* Fx65 *)


let byte_to_instruction = fun _ -> NOOP