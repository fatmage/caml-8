open State
open Tsdl

val fetch_decode_execute : c8_state -> c8_state
val timer_frame_ratio : int

val interpreter_loop : c8_state -> float (* last_time *) -> float (* time_sum *) -> int -> Sdl.renderer -> c8_state
val load_rom : in_channel -> c8_state