type c8_address ;;
type c8_register ;;
type c8_byte ;;
type c8_nibble ;;
type c8_two_bytes ;;


type instruction =
  | NOOP
  | SYS                                              (* 0nnn *)
  | CLS                                              (* 00E0 *)
  | RET                                              (* 00EE *)
  | JP      of c8_address                            (* 1nnn *)
  | CALL    of c8_address                            (* 2nnn *)
  | SE_rb   of c8_register * c8_byte                 (* 3xkk *)
  | SNE_rb  of c8_register * c8_byte                 (* 4xkk *)
  | SE_rr   of c8_register * c8_register             (* 5xy0 *)
  | LD_rb   of c8_register * c8_byte                 (* 6xkk *)
  | ADD_rb  of c8_register * c8_byte                 (* 7xkk *)
  | LD_rr   of c8_register * c8_register             (* 8xy0 *)
  | OR_rr   of c8_register * c8_register             (* 8xy1 *)
  | AND_rr  of c8_register * c8_register             (* 8xy2 *)
  | XOR_rr  of c8_register * c8_register             (* 8xy3 *)
  | ADD_rr  of c8_register * c8_register             (* 8xy4 *)
  | SUB_rr  of c8_register * c8_register             (* 8xy5 *)
  | SHR_rr  of c8_register * c8_register             (* 8xy6 *)
  | SUBN_rr of c8_register * c8_register             (* 8xy7 *)
  | SHL_rr  of c8_register * c8_register             (* 8xyE *)
  | SNE_rr  of c8_register * c8_register             (* 9xy0 *)
  | LD_i    of c8_address                            (* Annn *)
  | JP_0    of c8_address                            (* Bnnn *)
  | RND     of c8_register * c8_byte                 (* Cxkk *)
  | DRW     of c8_register * c8_register * c8_nibble (* Dxyn *)
  | SKP     of c8_register                           (* Ex9E *)
  | SKNP    of c8_register (*                           ExA1 *)
  | LD_dtr  of c8_register                           (* Fx07 *)
  | LD_key  of c8_register (*                           Fx0A *)
  | LD_rdt  of c8_register                           (* Fx15 *)
  | LD_str  of c8_register (*                           Fx18 *)
  | ADD_i   of c8_register                           (* Fx1E *)
  | LD_font of c8_register (*                           Fx29 *)
  | LD_mem  of c8_register                           (* Fx33 *)
  | LD_memi of c8_register (*                           Fx55 *)
  | LD_regi of c8_register ;;                        (* Fx65 *)

type memory = MemNode of memory * memory
            | MemLeaf of int ;; (* uint8t? byte? char? *)

let chip8_mem =
  let rec mem_help n =
    match n with
      | 0 -> MemLeaf (0)
      | x -> MemNode (mem_help (n - 1), mem_help (n - 1))
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

let fetch_instruction mem pc =
  let rec fetch_bytes mem pc key =
    if pc mod 2 == 0 then
      match mem with
        | MemLeaf _ -> failwith "too greedily, too deep"
        | MemNode (l, r) -> match l, r with 
          | MemNode (ll, rl), MemNode (lr, rr) -> if pc < key then fetch_bytes l pc (key / 2) 
                                                              else fetch_bytes r pc (key + (key / 2))
          | MemLeaf il, MemLeaf ir -> (il * 256) + ir
          | _, _ -> failwith "WAT"
  else
      match mem with
        | MemLeaf _ -> failwith "WAT"
        | MemNode (l, r) -> match l, r with
          | MemNode (MemLeaf ll, MemLeaf rl), MemNode (MemLeaf lr, MemLeaf rr) ->  (rl * 256) + lr 
          | _, _ -> if pc < key then fetch_bytes l pc (key / 2)
                                  else fetch_bytes r pc (key + (key / 2))
  in
    fetch_bytes mem pc 2048 ;;


let bytecode_to_instruction : int -> instruction = fun x -> NOOP ;;


    

  