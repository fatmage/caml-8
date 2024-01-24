open Inttypes

type c8_key = Pressed | NotPressed
type c8_keypad 

val keypad_empty : c8_keypad

val get_key : c8_keypad -> uint8 -> c8_key
val set_key : c8_keypad -> uint8 -> c8_key -> c8_keypad