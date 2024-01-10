
type memory

val chip8_mem : memory

val get_byte : memory -> int -> int
val set_byte : memory -> int -> int -> memory
val fetch_instruction: memory -> int -> int




