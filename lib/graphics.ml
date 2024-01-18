open Tsdl
open Display
open Inttypes


let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x


let init_graphics () = 
  Sdl.init Sdl.Init.video |> or_exit;
  let w = Sdl.create_window ~w:960 ~h:480 "caml-8" Sdl.Window.opengl |> or_exit in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer



let clear_graphics renderer =
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let rec draw_line line renderer col row =
  match line with 
    | [] -> ()
    | p :: ps ->  match p with 
                    | PixelOn  -> let rect = Sdl.Rect.create ~x:(15 * col) ~y:(15 * row)  ~w:15 ~h:15 in 
                                  Sdl.render_fill_rect renderer (Some rect) |> or_exit;
                                  draw_line ps renderer (col + 1) row
                    | PixelOff -> draw_line ps renderer (col + 1) row

        
let draw_graphics display renderer =
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  let rec draw_lines display row =
    match display with
      | [] -> ()
      | l :: ls -> draw_line l renderer 0 row;
                   draw_lines ls (row + 1) in
  
  draw_lines display 0;
  Sdl.render_present renderer



type events = {exit : bool; space_pressed : bool; left_pressed : bool; right_pressed : bool; keypad_down : (uint8 list); keypad_up : (uint8 list)}


let handle_event cpu curr_events =
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
        | `K1     -> {curr_events with keypad_down =  (U8.of_int 0x1) :: curr_events.keypad_down}
        | `K2     -> {curr_events with keypad_down =  (U8.of_int 0x2) :: curr_events.keypad_down}
        | `K3     -> {curr_events with keypad_down =  (U8.of_int 0x3) :: curr_events.keypad_down}
        | `K4     -> {curr_events with keypad_down =  (U8.of_int 0xC) :: curr_events.keypad_down}
        | `Q      -> {curr_events with keypad_down =  (U8.of_int 0x4) :: curr_events.keypad_down}
        | `W      -> {curr_events with keypad_down =  (U8.of_int 0x5) :: curr_events.keypad_down}
        | `E      -> {curr_events with keypad_down =  (U8.of_int 0x6) :: curr_events.keypad_down}
        | `R      -> {curr_events with keypad_down =  (U8.of_int 0xD) :: curr_events.keypad_down}
        | `A      -> {curr_events with keypad_down =  (U8.of_int 0x7) :: curr_events.keypad_down}
        | `S      -> {curr_events with keypad_down =  (U8.of_int 0x8) :: curr_events.keypad_down}
        | `D      -> {curr_events with keypad_down =  (U8.of_int 0x9) :: curr_events.keypad_down}
        | `F      -> {curr_events with keypad_down =  (U8.of_int 0xE) :: curr_events.keypad_down}
        | `Z      -> {curr_events with keypad_down =  (U8.of_int 0xA) :: curr_events.keypad_down}
        | `X      -> {curr_events with keypad_down =  (U8.of_int 0x0) :: curr_events.keypad_down}
        | `C      -> {curr_events with keypad_down =  (U8.of_int 0xB) :: curr_events.keypad_down}
        | `V      -> {curr_events with keypad_down =  (U8.of_int 0xF) :: curr_events.keypad_down}

        | _       ->  curr_events
      end
    | `Key_up -> 
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with 
      | `K1     -> {curr_events with keypad_down =  (U8.of_int 0x1) :: curr_events.keypad_down}
      | `K2     -> {curr_events with keypad_down =  (U8.of_int 0x2) :: curr_events.keypad_down}
      | `K3     -> {curr_events with keypad_down =  (U8.of_int 0x3) :: curr_events.keypad_down}
      | `K4     -> {curr_events with keypad_down =  (U8.of_int 0xC) :: curr_events.keypad_down}
      | `Q      -> {curr_events with keypad_down =  (U8.of_int 0x4) :: curr_events.keypad_down}
      | `W      -> {curr_events with keypad_down =  (U8.of_int 0x5) :: curr_events.keypad_down}
      | `E      -> {curr_events with keypad_down =  (U8.of_int 0x6) :: curr_events.keypad_down}
      | `R      -> {curr_events with keypad_down =  (U8.of_int 0xD) :: curr_events.keypad_down}
      | `A      -> {curr_events with keypad_down =  (U8.of_int 0x7) :: curr_events.keypad_down}
      | `S      -> {curr_events with keypad_down =  (U8.of_int 0x8) :: curr_events.keypad_down}
      | `D      -> {curr_events with keypad_down =  (U8.of_int 0x9) :: curr_events.keypad_down}
      | `F      -> {curr_events with keypad_down =  (U8.of_int 0xE) :: curr_events.keypad_down}
      | `Z      -> {curr_events with keypad_down =  (U8.of_int 0xA) :: curr_events.keypad_down}
      | `X      -> {curr_events with keypad_down =  (U8.of_int 0x0) :: curr_events.keypad_down}
      | `C      -> {curr_events with keypad_down =  (U8.of_int 0xB) :: curr_events.keypad_down}
      | `V      -> {curr_events with keypad_down =  (U8.of_int 0xF) :: curr_events.keypad_down}
      | `Escape -> {curr_events with exit = true}
      | `Space  -> {curr_events with space_pressed = true}
      | `Left   -> {curr_events with left_pressed = true}
      | `Right  -> {curr_events with right_pressed = true}
      | _       ->  curr_events
    end
    | `Quit   -> {curr_events with exit = true}
    | _       -> curr_events
  end else curr_events