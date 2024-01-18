open State
open Inttypes
open Memory
open Instruction
open Keyboard
open Graphics
open Tsdl




let fetch_decode_execute : c8_state -> c8_state =
  fun state ->  let opcode : uint16 = fetch_opcode (get_mem state) (get_pc state) in
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


let timer_time = 1. /. 60.
let timer_frame_ratio = 3
let frame_time = timer_time /. (float_of_int timer_frame_ratio)

let rec interpreter_loop : c8_state -> float (* last_time *) -> float (* time_sum *) -> int -> Sdl.renderer -> c8_state =
  fun state last_time time_sum timer_tick renderer ->
    clear_graphics renderer ;
    draw_graphics (get_display state) renderer;
    let t = Sys.time () in
    let dt = (t -. last_time) +. time_sum in
    if (dt > frame_time) then 
      let keyboard_events = clear_events |> handle_events in
      if keyboard_events.exit then exit 0 else
      let new_state = update_keypad state keyboard_events.keypad_down keyboard_events.keypad_up in
      interpreter_loop (tick_cpu new_state timer_tick) t (dt -. frame_time) (if timer_tick == 1 then timer_frame_ratio else timer_tick - 1) renderer
    else
      interpreter_loop state t dt timer_tick renderer
    

let rec debugger_loop : c8_state -> int -> c8_state =
  fun state timer_tick -> debugger_loop state timer_tick
    (* let keyboard_events = clear_events |> handle_events in 
    if keyboard_events.exit then exit 0 else 
    if keyboard_events.space_pressed then (* switch to interpreter *)
    let t = Sys.time () in 
    interpreter_loop 
    else if keyboard_events.left_pressed then 
    else if keyboard_events.right_pressed then
    else debugger_loop (tick_cpu state timer_tick) (if timer_tick == 1 then timer_frame_ratio else timer_tick - 1) *)


let load_rom : in_channel -> c8_state = fun file_channel -> init_state file_channel




