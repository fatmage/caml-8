type c8_address = int

type c8_memory = 
 | MemNode of c8_memory * c8_memory
 | MemLeaf of int ;; (* uint8t? byte? char? *)

let init_mem =
  let rec mem_help n =
    match n with
      | 0 -> MemLeaf (0)
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
    helper mem index 2048 ;;

let set_byte mem index v =
  let rec helper mem index key v =
    match mem with
      | MemLeaf _ -> MemLeaf (v)
      | MemNode (l, r) ->
        if index < key then 
          MemNode ((helper l index (key / 2) v), r) else
          MemNode (l, (helper r index (key + (key / 2)) v))
  in
    helper mem index 2048 v ;;

let fetch_opcode mem pc =
  let rec fetch_bytes mem pc key =
    if pc mod 2 == 0 then
      match mem with
        | MemLeaf _ -> failwith "too greedily, too deep"
        | MemNode (l, r) -> match l, r with 
          | MemNode (_, _), MemNode (_, _) -> if pc < key then fetch_bytes l pc (key / 2) 
                                                              else fetch_bytes r pc (key + (key / 2))
          | MemLeaf il, MemLeaf ir -> (il * 256) + ir
          | _, _ -> failwith "WAT"
  else
      match mem with
        | MemLeaf _ -> failwith "WAT"
        | MemNode (l, r) -> match l, r with
          | MemNode (MemLeaf _, MemLeaf rl), MemNode (MemLeaf lr, MemLeaf _) ->  (rl * 256) + lr 
          | _, _ -> if pc < key then fetch_bytes l pc (key / 2)
                                  else fetch_bytes r pc (key + (key / 2))
  in
    fetch_bytes mem pc 2048 ;;



