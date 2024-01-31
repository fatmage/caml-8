open Caml_8_lib.Platform
open Caml_8_lib.Graphics
open Caml_8_lib.Sound
open Caml_8_lib.State_history


let () = 
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    prerr_endline "Usage : main <file name>";
    exit 2
  end;
  Random.self_init ();
  let rom_channel = open_in_bin argv.(1) in
  let window, renderer = init_graphics () in
  let _ = init_sound () in
  let m = load_music music_path in
  (* let t = Sys.time () in *)
  let s = load_rom rom_channel in
  beeper s m;
  let hs = add_to_history empty_history s in
  (* let _ = interpreter_loop hs s t 0.0 timer_frame_ratio renderer m in *)
  let _ = debugger_loop hs s timer_frame_ratio renderer m in
  close_graphics window renderer;
  close_sound m;
  exit 0
