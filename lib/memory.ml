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
  let rec helper mem index left right =
    match mem with 
      | MemLeaf i -> i
      | MemNode (l, r) ->
        let m = (left + right)/2 in
        if index < m then 
          helper l index left m else 
          helper r index m right
  in  
    helper mem (U16.to_int index) 0 4096 ;;

let set_byte mem index v =
  let rec helper mem index left right v =
    match mem with
      | MemLeaf _ -> MemLeaf (v)
      | MemNode (l, r) ->
        let m = (left + right)/2 in
        if index < m then 
          MemNode ((helper l index left m v), r) else
          MemNode (l, (helper r index m right v))
  in
    helper mem (U16.to_int index) 0 4096 v ;;

let glue_bytes b1 b2 = U16.add (U16.shl (u8_to_16 b1) (U16.of_int 8)) (u8_to_16 b2)

let fetch_opcode : c8_memory -> uint16 -> uint16 = fun mem pc ->
  let rec fetch_even mem pc left right =
    match mem with
    | MemLeaf _ -> failwith "too greedily, too deep"
    | MemNode (l, r) -> match l, r with 
      | MemNode (_, _), MemNode (_, _) -> 
        let m = (left + right)/2 in
        if pc < m then fetch_even l pc left m
          else fetch_even r pc m right
      | MemLeaf il, MemLeaf ir -> glue_bytes il ir
      | _, _ -> failwith "WAT"
  in let rec fetch_odd mem pc left right =
    match mem with
      | MemLeaf _ -> failwith "WAT"
      | MemNode (l, r) -> match l, r with
        | MemNode (MemLeaf _, MemLeaf rl), MemNode (MemLeaf lr, MemLeaf _) ->  glue_bytes rl lr 
        | _, _ ->
        let m = (left + right)/2 in
        if pc < m then fetch_odd l pc left m
          else fetch_odd r pc m right
  in let pc_int = U16.to_int pc in
    if pc_int mod 2 == 0 then 
      fetch_even mem pc_int 0 4096
    else
      fetch_odd mem pc_int 0 4096


let rec load_mem : c8_memory -> uint16 -> (uint8 list) -> c8_memory =
  fun mem addr values ->
    match values with
      | x :: xs -> load_mem (set_byte mem addr x) (U16.succ addr) xs
      | [] -> mem


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


let rec load_mem_from_file : c8_memory -> uint16 -> in_channel -> c8_memory =
  fun mem addr file -> try 
    let b = (U8.of_int (input_byte file)) in 
    let memset = (set_byte mem addr b) in
    load_mem_from_file memset (U16.succ addr) file 
    with End_of_file -> mem 




