open Memory
open Inttypes 

type c8_key
type c8_state 

val set_mem : c8_state -> c8_memory -> c8_state
val get_mem : c8_state -> c8_memory 
val set_pc : c8_state -> c8_address -> c8_state
val get_pc : c8_state -> c8_address
val tick_pc : c8_state -> c8_state