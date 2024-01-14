open Memory
open Inttypes 

type c8_key
type c8_state 

val set_mem : c8_state -> c8_memory -> c8_state
val get_mem : c8_state -> c8_memory 
val set_pc : c8_state -> uint16 -> c8_state
val get_pc : c8_state -> uint16
val tick_pc : c8_state -> c8_state

val hd_stack : c8_state -> uint16
val push_stack : c8_state -> uint16 -> c8_state
val pop_stack : c8_state -> c8_state

val get_reg : c8_state -> uint8 -> uint8
val set_reg : c8_state -> uint8 -> uint8 -> c8_state
val set_flag : c8_state -> uint8 -> c8_state
