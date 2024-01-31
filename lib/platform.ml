open State
open Inttypes
open Instruction
open Keyboard
open Graphics
open Sound
open Tsdl
open Tsdl_mixer
open State_history



let timer_time = 1. /. 60.
let timer_frame_ratio = 3
let frame_time = timer_time /. (float_of_int timer_frame_ratio)


let load_rom : in_channel -> c8_state = fun file_channel -> 
  init_state file_channel



let fetch_decode_execute : c8_state -> c8_state =
  fun state ->  let opcode : uint16 = fetch_opcode state in
                let instruction : c8_instruction = decode_opcode opcode in
                match instruction with
                  | NoArg op -> op state
                  | Reg   op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                  | Addr  op -> op state                    (U16.logand opcode (U16.of_int 0x0FFF))
                  | DReg  op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x00F0)) (U16.of_int 4))) 
                  | RegV  op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8          (U16.logand opcode (U16.of_int 0x00FF)))
                  | DRegV op -> op state (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x0F00)) (U16.of_int 8)))
                                         (u16_to_8 (U16.shr (U16.logand opcode (U16.of_int 0x00F0)) (U16.of_int 4))) 
                                         (u16_to_8          (U16.logand opcode (U16.of_int 0x000F))) 

                                         
let tick_cpu : c8_state -> int -> c8_state = fun state timer_tick ->
  match timer_tick with
    | 1 -> state |> tick_timers |> fetch_decode_execute
    | x -> fetch_decode_execute state

let rec interpreter_loop : state_history -> c8_state -> float -> float -> int -> Sdl.renderer -> Mixer.music -> c8_state =
  fun history state last_time time_sum timer_tick renderer music ->
    beeper state music;
    clear_graphics renderer;
    draw_graphics (get_display state) renderer;
    let t = Sys.time () in
    let dt = (t -. last_time) +. time_sum in
    if (dt > frame_time) then 
      let keyboard_events = clear_events |> handle_events in
      if keyboard_events.exit then 
        state 
      else if keyboard_events.space_pressed then 
        debugger_loop history state timer_tick renderer music
      else
        let state_with_keypad = update_keypad state keyboard_events.keypad_down keyboard_events.keypad_up in
        let new_state = tick_cpu state_with_keypad timer_tick in
        interpreter_loop (add_to_history history new_state) new_state t (dt -. frame_time) (if timer_tick == 1 then timer_frame_ratio else timer_tick - 1) renderer music
    else
      interpreter_loop history state t dt timer_tick renderer music
and debugger_loop : state_history -> c8_state -> int -> Sdl.renderer -> Mixer.music -> c8_state =
  fun history state timer_tick renderer music -> 
    beeper state music;
    draw_graphics (get_display state) renderer;
    draw_debug_info state renderer;
    let keyboard_events = clear_events |> handle_events in 
    if keyboard_events.exit then 
      state 
    else if keyboard_events.space_pressed then (* switch to interpreter *)
      let t = Sys.time () in 
      interpreter_loop (purge_right history) state t 0.0 timer_tick renderer music
    else if keyboard_events.left_pressed then 
      let new_history = move_left history in 
      clear_graphics renderer;
      debugger_loop new_history (get_from_history new_history) timer_tick renderer music
    else if keyboard_events.right_pressed then 
      if history_up_to_date history then
        let new_state = tick_cpu state timer_tick in
        let new_history = add_to_history history new_state in 
        clear_graphics renderer;
        debugger_loop new_history new_state (if timer_tick == 1 then timer_frame_ratio else timer_tick - 1) renderer music
      else
        let new_history = move_right history in 
        clear_graphics renderer;
        debugger_loop new_history (get_from_history new_history) timer_tick renderer music
    else 
      debugger_loop history state timer_tick renderer music









