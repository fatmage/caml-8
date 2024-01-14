open Memory
open Inttypes


type instruction

val iNOOP    : instruction  (* 0nnn *)               
val iCLS     : instruction  (* 00E0 *) 
val iRET     : instruction  (* 00EE *)
val iJP      : instruction  (* 1nnn *)
val iCALL    : instruction  (* 2nnn *)
val iSE_rb   : instruction  (* 3xkk *)
val iSNE_rb  : instruction  (* 4xkk *)
val iSE_rr   : instruction  (* 5xy0 *)
val iLD_rb   : instruction  (* 6xkk *)
val iADD_rb  : instruction  (* 7xkk *)
val iLD_rr   : instruction  (* 8xy0 *)
val iOR_rr   : instruction  (* 8xy1 *)
val iAND_rr  : instruction  (* 8xy2 *)
val iXOR_rr  : instruction  (* 8xy3 *)
val iADD_rr  : instruction  (* 8xy4 *)
val iSUB_rr  : instruction  (* 8xy5 *)
val iSHR_rr  : instruction  (* 8xy6 *)
val iSUBN_rr : instruction  (* 8xy7 *)
val iSHL_rr  : instruction  (* 8xyE *)
val iSNE_rr  : instruction  (* 9xy0 *)
val iLD_i    : instruction  (* Annn *)
val iJP_0    : instruction  (* Bnnn *)
val iRND     : instruction  (* Cxkk *)
val iDRW     : instruction  (* Dxyn *)
val iSKP     : instruction  (* Ex9E *)
val iSKNP    : instruction  (* ExA1 *)
val iLD_dtr  : instruction  (* Fx07 *)
val iLD_key  : instruction  (* Fx0A *)
val iLD_rdt  : instruction  (* Fx15 *)
val iLD_str  : instruction  (* Fx18 *)
val iADD_i   : instruction  (* Fx1E *)
val iLD_font : instruction  (* Fx29 *)
val iLD_mem  : instruction  (* Fx33 *)
val iLD_memi : instruction  (* Fx55 *)
val iLD_regi : instruction  (* Fx65 *) 

val fetch_instruction : c8_memory -> c8_address -> instruction