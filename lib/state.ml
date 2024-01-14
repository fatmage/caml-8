open Memory
open Inttypes


type c8_pixel = PixelOn | PixelOff
type c8_display = (c8_pixel list) list

let empty_display : c8_display = 
  let rec make_row w = match w with
    | 0 -> []
    | x -> PixelOff :: (make_row (w - 1))
  in let rec make_disp h = match h with
    | 0 -> []
    | x -> (make_row 64) :: (make_disp (h - 1))
  in
    make_disp 32


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
  display : c8_display; stack : (uint16 list); vI      : uint16;
  v0 : uint8; v1 : uint8; v2 : uint8; v3 : uint8;
  v4 : uint8; v5 : uint8; v6 : uint8; v7 : uint8;
  v8 : uint8; v9 : uint8; vA : uint8; vB : uint8;
  vC : uint8; vD : uint8; vE : uint8; vF : uint8;
  dT : uint8; sT : uint8; iR : U16.t
}



let set_mem : c8_state -> c8_memory -> c8_state = fun s -> fun m -> {s with memory = m}
let get_mem : c8_state -> c8_memory = fun s -> s.memory
let set_pc  : c8_state -> uint16 -> c8_state = fun s -> fun addr -> {s with pc = addr}
let tick_pc : c8_state -> c8_state = fun s -> {s with pc = U16.succ (U16.succ s.pc) }
let get_pc  : c8_state -> uint16 = fun s -> s.pc

let init_state = {
  memory = init_mem; pc = U16.zero; keypad = keypad_empty; 
  display = empty_display; stack = []; vI = U16.zero;
  v0 = U8.zero ; v1 = U8.zero; v2 = U8.zero; v3 = U8.zero;
  v4 = U8.zero ; v5 = U8.zero; v6 = U8.zero; v7 = U8.zero;
  v8 = U8.zero ; v9 = U8.zero; vA = U8.zero; vB = U8.zero;
  vC = U8.zero ; vD = U8.zero; vE = U8.zero; vF = U8.zero;
  dT = U8.zero ; sT = U8.zero; iR = U16.zero}

let hd_stack : c8_state -> uint16 = fun s -> List.hd s.stack ;;
let push_stack : c8_state -> uint16 -> c8_state = fun s -> fun a -> {s with stack = a :: s.stack}
let pop_stack : c8_state -> c8_state = fun s -> {s with stack = List.tl s.stack}


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



















