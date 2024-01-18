open Inttypes

type events = {exit : bool; space_pressed : bool; left_pressed : bool; right_pressed : bool; keypad_down : (uint8 list); keypad_up : (uint8 list)}


val handle_events : events -> events
val clear_events : events

