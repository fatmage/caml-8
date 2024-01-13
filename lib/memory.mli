open Inttypes

type c8_memory

val init_mem : c8_memory

val get_byte : c8_memory -> c8_address -> c8_byte
val set_byte : c8_memory -> c8_address -> c8_byte -> c8_memory
val fetch_opcode: c8_memory -> c8_address -> c8_opcode





