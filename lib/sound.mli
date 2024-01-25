open State
open Tsdl_mixer

val music_path : string 

val init_sound : unit -> unit
val close_sound : Mixer.music -> unit
val load_music : string -> Mixer.music
val beeper : c8_state -> Mixer.music -> unit




