open Inttypes
open Tsdl

type events = {exit : bool; space_pressed : bool; left_pressed : bool; right_pressed : bool; keypad_down : (uint8 list); keypad_up : (uint8 list)}


let rec handle_events : events -> events = fun curr_events ->
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
        | `K1     -> {curr_events with keypad_down =  (U8.of_int 0x1) :: curr_events.keypad_down} |> handle_events 
        | `K2     -> {curr_events with keypad_down =  (U8.of_int 0x2) :: curr_events.keypad_down} |> handle_events 
        | `K3     -> {curr_events with keypad_down =  (U8.of_int 0x3) :: curr_events.keypad_down} |> handle_events 
        | `K4     -> {curr_events with keypad_down =  (U8.of_int 0xC) :: curr_events.keypad_down} |> handle_events 
        | `Q      -> {curr_events with keypad_down =  (U8.of_int 0x4) :: curr_events.keypad_down} |> handle_events 
        | `W      -> {curr_events with keypad_down =  (U8.of_int 0x5) :: curr_events.keypad_down} |> handle_events 
        | `E      -> {curr_events with keypad_down =  (U8.of_int 0x6) :: curr_events.keypad_down} |> handle_events 
        | `R      -> {curr_events with keypad_down =  (U8.of_int 0xD) :: curr_events.keypad_down} |> handle_events 
        | `A      -> {curr_events with keypad_down =  (U8.of_int 0x7) :: curr_events.keypad_down} |> handle_events 
        | `S      -> {curr_events with keypad_down =  (U8.of_int 0x8) :: curr_events.keypad_down} |> handle_events 
        | `D      -> {curr_events with keypad_down =  (U8.of_int 0x9) :: curr_events.keypad_down} |> handle_events 
        | `F      -> {curr_events with keypad_down =  (U8.of_int 0xE) :: curr_events.keypad_down} |> handle_events 
        | `Z      -> {curr_events with keypad_down =  (U8.of_int 0xA) :: curr_events.keypad_down} |> handle_events 
        | `X      -> {curr_events with keypad_down =  (U8.of_int 0x0) :: curr_events.keypad_down} |> handle_events 
        | `C      -> {curr_events with keypad_down =  (U8.of_int 0xB) :: curr_events.keypad_down} |> handle_events 
        | `V      -> {curr_events with keypad_down =  (U8.of_int 0xF) :: curr_events.keypad_down} |> handle_events 
        | _       ->  curr_events                                                                 |> handle_events 
      end
    | `Key_up -> 
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with 
      | `K1     -> {curr_events with keypad_down =  (U8.of_int 0x1) :: curr_events.keypad_down} |> handle_events 
      | `K2     -> {curr_events with keypad_down =  (U8.of_int 0x2) :: curr_events.keypad_down} |> handle_events 
      | `K3     -> {curr_events with keypad_down =  (U8.of_int 0x3) :: curr_events.keypad_down} |> handle_events 
      | `K4     -> {curr_events with keypad_down =  (U8.of_int 0xC) :: curr_events.keypad_down} |> handle_events 
      | `Q      -> {curr_events with keypad_down =  (U8.of_int 0x4) :: curr_events.keypad_down} |> handle_events 
      | `W      -> {curr_events with keypad_down =  (U8.of_int 0x5) :: curr_events.keypad_down} |> handle_events 
      | `E      -> {curr_events with keypad_down =  (U8.of_int 0x6) :: curr_events.keypad_down} |> handle_events 
      | `R      -> {curr_events with keypad_down =  (U8.of_int 0xD) :: curr_events.keypad_down} |> handle_events 
      | `A      -> {curr_events with keypad_down =  (U8.of_int 0x7) :: curr_events.keypad_down} |> handle_events 
      | `S      -> {curr_events with keypad_down =  (U8.of_int 0x8) :: curr_events.keypad_down} |> handle_events 
      | `D      -> {curr_events with keypad_down =  (U8.of_int 0x9) :: curr_events.keypad_down} |> handle_events 
      | `F      -> {curr_events with keypad_down =  (U8.of_int 0xE) :: curr_events.keypad_down} |> handle_events 
      | `Z      -> {curr_events with keypad_down =  (U8.of_int 0xA) :: curr_events.keypad_down} |> handle_events 
      | `X      -> {curr_events with keypad_down =  (U8.of_int 0x0) :: curr_events.keypad_down} |> handle_events 
      | `C      -> {curr_events with keypad_down =  (U8.of_int 0xB) :: curr_events.keypad_down} |> handle_events 
      | `V      -> {curr_events with keypad_down =  (U8.of_int 0xF) :: curr_events.keypad_down} |> handle_events 
      | `Escape -> {curr_events with exit = true}                                               |> handle_events                              
      | `Space  -> {curr_events with space_pressed = true}                                      |> handle_events 
      | `Left   -> {curr_events with left_pressed = true}                                       |> handle_events 
      | `Right  -> {curr_events with right_pressed = true}                                      |> handle_events 
      | _       ->  curr_events                                                                 |> handle_events 
    end
    | `Quit   -> {curr_events with exit = true}                                                 |> handle_events 
    | _       -> curr_events                                                                    |> handle_events 
  end else curr_events


  let clear_events : events = { exit = false; space_pressed = false; left_pressed = false; right_pressed = false;
                               keypad_down = []; keypad_up = [] }
