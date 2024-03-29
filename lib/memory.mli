open Inttypes

type c8_memory

val init_mem : c8_memory

val get_byte : c8_memory -> uint16 -> uint8
val set_byte : c8_memory -> uint16 -> uint8 -> c8_memory
val get_current_opcode: c8_memory -> uint16 -> uint16
val load_mem_from_file : c8_memory -> uint16 -> in_channel -> c8_memory

