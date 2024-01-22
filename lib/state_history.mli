open State

type state_history 

val empty_history : state_history

val move_to_end : state_history -> state_history 
val purge_right : state_history -> state_history

val move_left : state_history -> state_history

val move_right : state_history -> state_history

val get_from_history : state_history -> c8_state 

val add_to_history : state_history -> c8_state -> state_history 







