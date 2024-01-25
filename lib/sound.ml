open Tsdl
open Tsdl_mixer
open Inttypes
open State



let or_exit : ('a, [< `Msg of string ]) result -> 'a = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let ignore_or_exit : ('a, [< `Msg of string ]) result -> unit = function
| Error (`Msg e) -> Sdl.log "%s" e; exit 1
| Ok x -> ()


let music_path : string = "assets/200.mp3"


let init_sound : unit -> unit = 
  fun _ ->
    Sdl.init Sdl.Init.audio |> or_exit;
    Mixer.init Mixer.Init.mp3 |> ignore_or_exit;
    Mixer.open_audio 44100 Mixer.default_format 2 2048 |> or_exit



let load_music : string -> Mixer.music = 
  fun str ->
    Mixer.load_mus str |> or_exit


let close_sound : Mixer.music -> unit = 
  fun m ->
    Mixer.free_music m;
    Mixer.quit ()

let beeper : c8_state -> Mixer.music -> unit = 
  fun state beep_music ->
    let timer : uint8 = state |> get_st in
    let pause_resume : uint8 -> unit = 
      fun t ->
        match (t |> U8.to_int), (() |> Mixer.paused_music) with 
        | 0, true -> ()
        | 0, false -> Mixer.pause_music ()
        | x, true -> Mixer.resume_music ()
        | x, false -> ()

    in
    match (() |> Mixer.playing_music) with 
      | true  -> pause_resume timer
      | false -> Mixer.play_music beep_music (-1) |> ignore_or_exit




