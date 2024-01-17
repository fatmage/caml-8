open Inttypes

type c8_memory

val init_mem : c8_memory

val get_byte : c8_memory -> uint16 -> uint8
val set_byte : c8_memory -> uint16 -> uint8 -> c8_memory
val fetch_opcode: c8_memory -> uint16 -> uint16
val load_mem : c8_memory -> uint16 -> (uint8 list) -> c8_memory

