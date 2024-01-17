open Inttypes


type c8_memory = 
 | MemNode of c8_memory * c8_memory
 | MemLeaf of uint8 

let empty_mem =
  let rec mem_help n =
    match n with
      | 0 -> MemLeaf (U8.zero)
      | _ -> MemNode (mem_help (n - 1), mem_help (n - 1))
  in 
    mem_help 12 ;;

let get_byte mem index =
  let rec helper mem index key =
    match mem with 
      | MemLeaf i -> i
      | MemNode (l, r) ->
        if index < key then helper l index (key / 2) else helper r index (key + (key / 2))
  in  
    helper mem (U16.to_int index) 2048 ;;

let set_byte mem index v =
  let rec helper mem index key v =
    match mem with
      | MemLeaf _ -> MemLeaf (v)
      | MemNode (l, r) ->
        if index < key then 
          MemNode ((helper l index (key / 2) v), r) else
          MemNode (l, (helper r index (key + (key / 2)) v))
  in
    helper mem (U16.to_int index) 2048 v ;;

let glue_bytes b1 b2 = U16.add (U16.shl (u8_to_16 b1) (U16.of_int 8)) (u8_to_16 b2)

let fetch_opcode mem pc =
  let rec fetch_even mem pc key =
    match mem with
    | MemLeaf _ -> failwith "too greedily, too deep"
    | MemNode (l, r) -> match l, r with 
      | MemNode (_, _), MemNode (_, _) -> if pc < key then fetch_even l pc (key / 2) 
                                                          else fetch_even r pc (key + (key / 2))
      | MemLeaf il, MemLeaf ir -> glue_bytes il ir
      | _, _ -> failwith "WAT"
  in let rec fetch_odd mem pc key =
    match mem with
      | MemLeaf _ -> failwith "WAT"
      | MemNode (l, r) -> match l, r with
        | MemNode (MemLeaf _, MemLeaf rl), MemNode (MemLeaf lr, MemLeaf _) ->  glue_bytes rl lr 
        | _, _ -> if pc < key then fetch_odd l pc (key / 2)
                                else fetch_odd r pc (key + (key / 2))
  in let pc_int = U16.to_int pc in
    if pc_int mod 2 == 0 then 
      fetch_even mem pc_int 2048
    else
      fetch_odd mem pc_int 2048


let rec load_mem : c8_memory -> uint16 -> (uint8 list) -> c8_memory =
  fun mem addr values ->
    match values with
      | [] -> mem
      | x :: xs -> load_mem (set_byte mem addr x) (U16.succ addr) xs

let init_mem : c8_memory = 
  let fontset =  [(U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0x90); (U8.of_int 0x90); (U8.of_int 0xF0);    (*0*)
                  (U8.of_int 0x20); (U8.of_int 0x60); (U8.of_int 0x20); (U8.of_int 0x20); (U8.of_int 0x70);    (*1*)
                  (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0);    (*2*)
                  (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0xF0);    (*3*)
                  (U8.of_int 0x90); (U8.of_int 0x90); (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0x10);    (*4*)
                  (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0xF0);    (*5*)
                  (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0xF0);    (*6*)
                  (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0x20); (U8.of_int 0x40); (U8.of_int 0x40);    (*7*)
                  (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0xF0);    (*8*)
                  (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0xF0); (U8.of_int 0x10); (U8.of_int 0xF0);    (*9*)
                  (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0xF0); (U8.of_int 0x90); (U8.of_int 0x90);    (*A*)
                  (U8.of_int 0xE0); (U8.of_int 0x90); (U8.of_int 0xE0); (U8.of_int 0x90); (U8.of_int 0xE0);    (*B*)
                  (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0x80); (U8.of_int 0x80); (U8.of_int 0xF0);    (*C*)
                  (U8.of_int 0xE0); (U8.of_int 0x90); (U8.of_int 0x90); (U8.of_int 0x90); (U8.of_int 0xE0);    (*D*)
                  (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0);    (*E*)
                  (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0xF0); (U8.of_int 0x80); (U8.of_int 0x80)] in (*F*)
load_mem empty_mem U16.zero fontset


