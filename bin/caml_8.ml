open Caml_8_lib.Platform
open Caml_8_lib.Graphics


let () = 
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    prerr_endline "Usage : main <file name>";
    exit 2
  end;
  let rom_channel = open_in_bin argv.(1) in
  let renderer = init_graphics () in
  let t = Sys.time () in
  let _ = interpreter_loop (load_rom rom_channel) t 0.0 timer_frame_ratio renderer in
  exit 0
