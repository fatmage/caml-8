open State
open Inttypes
open Memory
open Instruction




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



let tick_cpu : c8_state -> uint8 -> c8_state = fun state timer_tick ->
  match (U8.to_int timer_tick) with
    | 0 -> state |> tick_timers |> fetch_decode_execute
    | x -> fetch_decode_execute state


let timer_time = 1. /. 60.
let timer_frame_ratio = U8.of_int 3
let frame_time = timer_time /. (float_of_int (U8.to_int timer_frame_ratio))

let rec interpreter_loop : c8_state -> float (* last_time *) -> float (* time_sum *) -> uint8 -> c8_state =
  fun state last_time time_sum timer_tick ->
    let t = Sys.time () in
    let dt = (t -. last_time) +. time_sum in
    if (dt > frame_time) then 
      interpreter_loop (tick_cpu state timer_tick) t (dt -. frame_time) (U8.pred timer_tick)
    else
      interpreter_loop state t dt timer_tick
    

let rec debugger_loop : c8_state -> uint8 -> c8_state =
  fun state timer_tick ->
    debugger_loop (tick_cpu state timer_tick) (U8.pred timer_tick)




