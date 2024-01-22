open Caml_8_lib.Platform
open Caml_8_lib.Graphics
open Caml_8_lib.State_history


let () = 
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    prerr_endline "Usage : main <file name>";
    exit 2
  end;
  Random.self_init ();
  let rom_channel = open_in_bin argv.(1) in
  let renderer = init_graphics () in
  let t = Sys.time () in
  let s = load_rom rom_channel in
  let hs = add_to_history empty_history s in
  let _ = interpreter_loop hs s t 0.0 timer_frame_ratio renderer in
  exit 0
