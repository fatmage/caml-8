open Memory
open Inttypes 

type c8_key = Pressed | NotPressed
type c8_state 

val clear_disp : c8_state -> c8_state

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
val add_reg : c8_state -> uint8 -> uint8 -> c8_state 
val sub_reg : c8_state -> uint8 -> uint8 -> c8_state  

val get_ir : c8_state -> uint16 
val set_ir : c8_state -> uint16 -> c8_state
val add_ir : c8_state -> uint16 -> c8_state

val get_key : c8_state -> uint8 -> c8_key

val get_dt : c8_state -> uint8
val get_st : c8_state -> uint8

val set_dt : c8_state -> uint8 -> c8_state
val set_st : c8_state -> uint8 -> c8_state

val find_pressed : c8_state -> uint8

val update_mem : c8_state -> uint16 -> uint8 -> c8_state
val fetch_mem : c8_state -> uint16 -> uint8 