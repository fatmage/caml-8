open Memory
open Inttypes


type instruction

val iNOOP    : instruction
val iSYS     : instruction
val iCLS     : instruction
val iRET     : instruction
val iJP      : instruction
val iCALL    : instruction
val iSE_rb   : instruction
val iSNE_rb  : instruction
val iSE_rr   : instruction
val iLD_rb   : instruction
val iADD_rb  : instruction
val iLD_rr   : instruction
val iOR_rr   : instruction
val iAND_rr  : instruction
val iXOR_rr  : instruction
val iADD_rr  : instruction
val iSUB_rr  : instruction
val iSHR_rr  : instruction
val iLD_i    : instruction
val iJP_0    : instruction
val iRND     : instruction
val iDRW     : instruction
val iSKP     : instruction
val iSKNP    : instruction
val iLD_dtr  : instruction
val iLD_key  : instruction
val iLD_rdt  : instruction
val iLD_str  : instruction
val iADD_i   : instruction
val iLD_font : instruction
val iLD_mem  : instruction
val iLD_memi : instruction
val iLD_regi : instruction

val fetch_instruction : c8_memory -> c8_address -> instruction