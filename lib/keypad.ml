open Inttypes

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

let get_key : c8_keypad -> uint8 -> c8_key = 
  fun keypad key -> 
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


let set_key : c8_keypad -> uint8 -> c8_key -> c8_keypad =
  fun keypad key v ->
    match U8.to_int key with 
      | 0x0 -> {keypad with k0 = v}
      | 0x1 -> {keypad with k1 = v}
      | 0x2 -> {keypad with k2 = v}
      | 0x3 -> {keypad with k3 = v}
      | 0x4 -> {keypad with k4 = v}
      | 0x5 -> {keypad with k5 = v}
      | 0x6 -> {keypad with k6 = v}
      | 0x7 -> {keypad with k7 = v}
      | 0x8 -> {keypad with k8 = v}
      | 0x9 -> {keypad with k9 = v}
      | 0xA -> {keypad with kA = v}
      | 0xB -> {keypad with kB = v}
      | 0xC -> {keypad with kC = v}
      | 0xD -> {keypad with kD = v}
      | 0xE -> {keypad with kE = v}
      | 0xF -> {keypad with kF = v}
      | _ -> failwith "Wrong key number"
