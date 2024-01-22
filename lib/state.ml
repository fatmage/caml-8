open Memory
open Inttypes
open Display

type c8_key = Pressed | NotPressed

type c8_keypad = {
  k0 : c8_key; k1 : c8_key; k2 : c8_key; k3 : c8_key;
  k4 : c8_key; k5 : c8_key; k6 : c8_key; k7 : c8_key;
  k8 : c8_key; k9 : c8_key; kA : c8_key; kB : c8_key;
  kC : c8_key; kD : c8_key; kE : c8_key; kF : c8_key;
}

let keypad_empty : c8_keypad = {
  k0 = NotPressed; k1 = NotPressed; k2 = NotPressed; k3 = NotPressed;
  k4 = NotPressed; k5 = NotPressed; k6 = NotPressed; k7 = NotPressed;
  k8 = NotPressed; k9 = NotPressed; kA = NotPressed; kB = NotPressed;
  kC = NotPressed; kD = NotPressed; kE = NotPressed; kF = NotPressed;
}


type c8_state = {
  memory  : c8_memory;  pc    : uint16;        keypad : c8_keypad; 
  display : c8_display; stack : (uint16 list); 
  v0 : uint8; v1 : uint8; v2 : uint8; v3 : uint8;
  v4 : uint8; v5 : uint8; v6 : uint8; v7 : uint8;
  v8 : uint8; v9 : uint8; vA : uint8; vB : uint8;
  vC : uint8; vD : uint8; vE : uint8; vF : uint8;
  dT : uint8; sT : uint8; vI      : uint16;
}

let init_state = fun file_channel -> {
  memory = load_mem_from_file init_mem (U16.of_int 512) file_channel; pc = (U16.of_int 512); keypad = keypad_empty; 
  display = empty_display; stack = [];
  v0 = U8.zero ; v1 = U8.zero; v2 = U8.zero; v3 = U8.zero;
  v4 = U8.zero ; v5 = U8.zero; v6 = U8.zero; v7 = U8.zero;
  v8 = U8.zero ; v9 = U8.zero; vA = U8.zero; vB = U8.zero;
  vC = U8.zero ; vD = U8.zero; vE = U8.zero; vF = U8.zero;
  dT = U8.zero ; sT = U8.zero; vI = U16.zero; }

(*  ========================  DISPLAY  ========================  *)

let rec get_sprite : c8_state -> uint16 -> uint8 -> uint8 -> ((c8_pixel list) list) =
  fun state addr col height ->
    match (U8.to_int height) with
      | 0 -> []
      | x ->  let byte = get_byte state.memory addr in
              let byte_line = byte_to_line byte col in
              byte_line :: (get_sprite state (U16.succ addr) col (U8.pred height))


let draw_sprite : c8_state -> uint16 -> uint8 -> uint8 -> uint8 -> c8_state =
  fun state addr row col height -> {state with display = draw_sprite_display state.display (get_sprite state addr col height) row}

let clear_disp : c8_state -> c8_state = fun s -> {s with display = empty_display}

let get_display : c8_state -> c8_display = fun state -> state.display
              
(*  ========================  KEYPAD  ========================  *)


let get_key : c8_keypad -> uint8 -> c8_key = fun keypad key -> 
  match U8.to_int key with
    | 0x0 -> keypad.k0
    | 0x1 -> keypad.k1
    | 0x2 -> keypad.k2
    | 0x3 -> keypad.k3
    | 0x4 -> keypad.k4
    | 0x5 -> keypad.k5
    | 0x6 -> keypad.k6
    | 0x7 -> keypad.k7
    | 0x8 -> keypad.k8
    | 0x9 -> keypad.k9
    | 0xA -> keypad.kA
    | 0xB -> keypad.kB
    | 0xC -> keypad.kC
    | 0xD -> keypad.kD
    | 0xE -> keypad.kE
    | 0xF -> keypad.kF
    | _ -> failwith "Wrong key number"

let check_key : c8_state -> uint8 -> c8_key = fun state key -> 
  print_string (U8.to_string key);
  print_string " to byl klawisz\n";
   get_key state.keypad key

let press_key : c8_keypad -> uint8 -> c8_keypad = fun keypad key ->
  match U8.to_int key with
    | 0x0 -> {keypad with k0 = (if keypad.k0 == NotPressed then Pressed else keypad.k0)}
    | 0x1 -> {keypad with k1 = (if keypad.k1 == NotPressed then Pressed else keypad.k1)}
    | 0x2 -> {keypad with k2 = (if keypad.k2 == NotPressed then Pressed else keypad.k2)}
    | 0x3 -> {keypad with k3 = (if keypad.k3 == NotPressed then Pressed else keypad.k3)}
    | 0x4 -> {keypad with k4 = (if keypad.k4 == NotPressed then Pressed else keypad.k4)}
    | 0x5 -> {keypad with k5 = (if keypad.k5 == NotPressed then Pressed else keypad.k5)}
    | 0x6 -> {keypad with k6 = (if keypad.k6 == NotPressed then Pressed else keypad.k6)}
    | 0x7 -> {keypad with k7 = (if keypad.k7 == NotPressed then Pressed else keypad.k7)}
    | 0x8 -> {keypad with k8 = (if keypad.k8 == NotPressed then Pressed else keypad.k8)}
    | 0x9 -> {keypad with k9 = (if keypad.k9 == NotPressed then Pressed else keypad.k9)}
    | 0xA -> {keypad with kA = (if keypad.kA == NotPressed then Pressed else keypad.kA)}
    | 0xB -> {keypad with kB = (if keypad.kB == NotPressed then Pressed else keypad.kB)}
    | 0xC -> {keypad with kC = (if keypad.kC == NotPressed then Pressed else keypad.kC)}
    | 0xD -> {keypad with kD = (if keypad.kD == NotPressed then Pressed else keypad.kD)}
    | 0xE -> {keypad with kE = (if keypad.kE == NotPressed then Pressed else keypad.kE)}
    | 0xF -> {keypad with kF = (if keypad.kF == NotPressed then Pressed else keypad.kF)}
    | _ -> failwith "Wrong key number"

let release_key : c8_keypad -> uint8 -> c8_keypad = fun keypad key ->
  match U8.to_int key with
    | 0x0 -> {keypad with k0 = (if keypad.k0 == Pressed then NotPressed else keypad.k0)}
    | 0x1 -> {keypad with k1 = (if keypad.k1 == Pressed then NotPressed else keypad.k1)}
    | 0x2 -> {keypad with k2 = (if keypad.k2 == Pressed then NotPressed else keypad.k2)}
    | 0x3 -> {keypad with k3 = (if keypad.k3 == Pressed then NotPressed else keypad.k3)}
    | 0x4 -> {keypad with k4 = (if keypad.k4 == Pressed then NotPressed else keypad.k4)}
    | 0x5 -> {keypad with k5 = (if keypad.k5 == Pressed then NotPressed else keypad.k5)}
    | 0x6 -> {keypad with k6 = (if keypad.k6 == Pressed then NotPressed else keypad.k6)}
    | 0x7 -> {keypad with k7 = (if keypad.k7 == Pressed then NotPressed else keypad.k7)}
    | 0x8 -> {keypad with k8 = (if keypad.k8 == Pressed then NotPressed else keypad.k8)}
    | 0x9 -> {keypad with k9 = (if keypad.k9 == Pressed then NotPressed else keypad.k9)}
    | 0xA -> {keypad with kA = (if keypad.kA == Pressed then NotPressed else keypad.kA)}
    | 0xB -> {keypad with kB = (if keypad.kB == Pressed then NotPressed else keypad.kB)}
    | 0xC -> {keypad with kC = (if keypad.kC == Pressed then NotPressed else keypad.kC)}
    | 0xD -> {keypad with kD = (if keypad.kD == Pressed then NotPressed else keypad.kD)}
    | 0xE -> {keypad with kE = (if keypad.kE == Pressed then NotPressed else keypad.kE)}
    | 0xF -> {keypad with kF = (if keypad.kF == Pressed then NotPressed else keypad.kF)}
    | _ -> failwith "Wrong key number"


let rec keypad_up : c8_keypad -> (uint8 list) -> c8_keypad =
  fun keypad released ->
    match released with 
      | [] -> keypad 
      | x :: xs -> keypad_up (release_key keypad x) xs

let rec keypad_down : c8_keypad -> (uint8 list) -> c8_keypad =
  fun keypad pressed ->
    match pressed with
      | [] -> keypad 
      | x :: xs -> keypad_down (press_key keypad x) xs

let update_keypad : c8_state -> (uint8 list) -> (uint8 list) -> c8_state =
  fun state pressed released -> {state with keypad = keypad_up (keypad_down state.keypad pressed) released}


let find_pressed : c8_state -> uint8 = fun state ->
  let rec find_help : c8_state -> uint8 -> uint8 = fun state keynum ->
    if U8.lte keynum (U8.of_int 0xF) then 
      match get_key state.keypad keynum with
        | Pressed -> keynum
        | NotPressed -> find_help state (U8.succ keynum)
    else keynum in
    find_help state U8.zero



(*  ========================  MEMORY  ========================  *)

let set_mem : c8_state -> c8_memory -> c8_state = fun s -> fun m -> {s with memory = m}
let get_mem : c8_state -> c8_memory = fun s -> s.memory

let update_mem : c8_state -> uint16 -> uint8 -> c8_state = fun state addr v -> {state with memory = set_byte state.memory addr v}
let fetch_mem : c8_state -> uint16 -> uint8 = fun state addr -> get_byte state.memory addr

(*  ========================  PROGRAM COUNTER  ========================  *)

let set_pc  : c8_state -> uint16 -> c8_state = fun s -> fun addr -> {s with pc = addr}
let tick_pc : c8_state -> c8_state = fun s -> {s with pc = U16.succ (U16.succ s.pc)}
let get_pc  : c8_state -> uint16 = fun s -> s.pc

(*  ========================  STACK  ========================  *)

let hd_stack : c8_state -> uint16 = fun s -> List.hd s.stack ;;
let stack_depth : c8_state -> uint8 = fun s -> 
  let rec depth_internal : (uint16 list) -> uint8 -> uint8 = fun stack acc-> 
    match stack with
      | [] -> acc
      | x :: xs -> depth_internal xs (U8.succ acc) in 
  depth_internal s.stack U8.zero
let push_stack : c8_state -> uint16 -> c8_state = fun s -> fun a -> {s with stack = a :: s.stack}
let pop_stack : c8_state -> c8_state = fun s -> {s with stack = List.tl s.stack}

(*  ========================  REGISTERS  ========================  *)

let get_reg : c8_state -> uint8 -> uint8 = fun state -> fun index -> match U8.to_int index with
  | 0x0 -> state.v0
  | 0x1 -> state.v1
  | 0x2 -> state.v2
  | 0x3 -> state.v3
  | 0x4 -> state.v4
  | 0x5 -> state.v5
  | 0x6 -> state.v6
  | 0x7 -> state.v7
  | 0x8 -> state.v8
  | 0x9 -> state.v9
  | 0xA -> state.vA
  | 0xB -> state.vB
  | 0xC -> state.vC
  | 0xD -> state.vD
  | 0xE -> state.vE
  | 0xF -> state.vF
  | _ -> failwith "Wrong register number"

let set_reg : c8_state -> uint8 -> uint8 -> c8_state = fun state -> fun index -> fun v -> match U8.to_int index with
  | 0x0 -> {state with v0 = v}
  | 0x1 -> {state with v1 = v}
  | 0x2 -> {state with v2 = v}
  | 0x3 -> {state with v3 = v}
  | 0x4 -> {state with v4 = v}
  | 0x5 -> {state with v5 = v}
  | 0x6 -> {state with v6 = v}
  | 0x7 -> {state with v7 = v}
  | 0x8 -> {state with v8 = v}
  | 0x9 -> {state with v9 = v}
  | 0xA -> {state with vA = v}
  | 0xB -> {state with vB = v}
  | 0xC -> {state with vC = v}
  | 0xD -> {state with vD = v}
  | 0xE -> {state with vE = v}
  | 0xF -> {state with vF = v}
  | _ -> failwith "Wrong register number"

let set_flag : c8_state -> uint8 -> c8_state = fun state -> fun v -> set_reg state (U8.of_int 0xF) v

let add_reg : c8_state -> uint8 -> uint8 -> c8_state  = fun state reg v -> set_reg state reg (U8.add (get_reg state reg) v)
let sub_reg : c8_state -> uint8 -> uint8 -> c8_state  = fun state reg v -> set_reg state reg (U8.sub (get_reg state reg) v)

let get_ir : c8_state -> uint16 = fun state -> state.vI
let set_ir : c8_state -> uint16 -> c8_state = fun state addr -> {state with vI = addr}
let add_ir : c8_state -> uint16 -> c8_state = fun state addr -> {state with vI = U16.add state.vI addr}

(*  ========================  TIMERS  ========================  *)
let get_dt : c8_state -> uint8 = fun state -> state.dT
let get_st : c8_state -> uint8 = fun state -> state.sT

let set_dt : c8_state -> uint8 -> c8_state = fun state v -> {state with dT = v}
let set_st : c8_state -> uint8 -> c8_state = fun state v -> {state with sT = v}

let tick_dt : c8_state -> c8_state = fun state -> {state with dT = if U8.gt state.dT U8.zero then U8.pred state.dT else state.dT}
let tick_st : c8_state -> c8_state = fun state -> {state with sT = if U8.gt state.sT  U8.zero then U8.pred state.sT else state.sT}

let tick_timers : c8_state -> c8_state = fun state -> state |> tick_dt |> tick_st
