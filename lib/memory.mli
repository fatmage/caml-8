type c8_address = int
type c8_memory

val init_mem : c8_memory

val get_byte : c8_memory -> int -> int
val set_byte : c8_memory -> int -> int -> c8_memory
val fetch_opcode: c8_memory -> c8_address -> int





