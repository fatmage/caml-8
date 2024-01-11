open Memory
type c8_register = int


type c8_key = bool

type c8_keypad = {
k0 : c8_key; k1 : c8_key; k2 : c8_key; k3 : c8_key;
k4 : c8_key; k5 : c8_key; k6 : c8_key; k7 : c8_key;
k8 : c8_key; k9 : c8_key; kA : c8_key; kB : c8_key;
kC : c8_key; kD : c8_key; kE : c8_key; kF : c8_key;
}

type c8_state = {memory : c8_memory; pc : c8_address;
 v0 : c8_register; v1 : c8_register; v2 : c8_register; v3 : c8_register;
 v4 : c8_register; v5 : c8_register; v6 : c8_register; v7 : c8_register;
 v8 : c8_register; v9 : c8_register; vA : c8_register; vB : c8_register;
 vC : c8_register; vD : c8_register; vE : c8_register; vF : c8_register;}


let set_mem : c8_state -> c8_memory -> c8_state = fun s -> fun m -> {s with memory = m}
let get_mem : c8_state -> c8_memory = fun s -> s.memory
let set_pc  : c8_state -> c8_address -> c8_state = fun s -> fun addr -> {s with pc = addr}
let tick_pc : c8_state -> c8_state = fun s -> {s with pc = s.pc + 2}
let get_pc = fun s -> s.pc



