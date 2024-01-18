open State
open Inttypes
open Memory
open Instruction


let fetch_decode_execute : c8_state -> c8_state =
  fun state ->  let opcode : uint16 = fetch_opcode (get_mem state) (get_pc state) in
                let instruction : c8_instruction = decode_opcode opcode in
                match instruction with
                  | NoArg op -> op state
                  | Reg   op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                  | Addr  op -> op state                    (U16.logand opcode (U16.of_int 0x0FFF))
                  | DReg  op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x00F0)) (U16.of_int 4))) 
                  | RegV  op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8          (U16.logand opcode (U16.of_int 0x00FF)))
                  | DRegV op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x00F0)) (U16.of_int 4))) 
                                         (u16_to_8          (U16.logand opcode (U16.of_int 0x000F))) 

