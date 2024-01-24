open Inttypes 
open Memory
open Display
open Keypad


type c8_state 

val init_state : in_channel -> c8_state

val clear_disp : c8_state -> c8_state
val get_display : c8_state -> c8_display
val draw_sprite : c8_state -> uint16 -> uint8 -> uint8 -> uint8 -> c8_state

val check_key : c8_state -> uint8 -> c8_key
val update_keypad : c8_state -> (uint8 list) -> (uint8 list) -> c8_state
val find_pressed : c8_state -> uint8

val set_mem : c8_state -> c8_memory -> c8_state
val get_mem : c8_state -> c8_memory 
val update_mem : c8_state -> uint16 -> uint8 -> c8_state
val fetch_mem : c8_state -> uint16 -> uint8 
val fetch_opcode: c8_state -> uint16

val set_pc : c8_state -> uint16 -> c8_state
val get_pc : c8_state -> uint16
val tick_pc : c8_state -> c8_state

val hd_stack : c8_state -> uint16
val stack_depth : c8_state -> uint8
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

val get_dt : c8_state -> uint8
val get_st : c8_state -> uint8
val set_dt : c8_state -> uint8 -> c8_state
val set_st : c8_state -> uint8 -> c8_state
val tick_timers : c8_state -> c8_state




