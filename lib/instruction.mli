open Inttypes
open State


type c8_instruction =
  | NoArg of (c8_state -> c8_state)
  | Reg   of (c8_state ->  uint8 ->  c8_state)
  | Addr  of (c8_state ->  uint16 ->  c8_state)
  | DReg  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | RegV  of (c8_state ->  uint8 ->  uint8 ->  c8_state)
  | DRegV of (c8_state ->  uint8 ->  uint8 ->  uint8 ->  c8_state)


val decode_opcode : uint16 -> c8_instruction

