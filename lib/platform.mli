open State
open Tsdl
open Tsdl_mixer
open State_history

val timer_frame_ratio : int

val load_rom : in_channel -> c8_state
val interpreter_loop : state_history -> c8_state -> float -> float -> int -> Sdl.renderer -> Mixer.music -> c8_state
