open State

type state_history = {left : (c8_state list); right : (c8_state list)}

let empty_history : state_history = {left = []; right = []}

let move_to_end : state_history -> state_history = fun sl -> {left = sl.left @ (List.rev sl.right); right = []}
let purge_right : state_history -> state_history = fun sl -> {sl with right = []}

let move_left : state_history -> state_history = fun sl -> 
  match sl.left with
    | x :: xs -> begin match xs with
      | y :: ys -> {left = xs; right = x :: sl.right}
      | [] -> sl
      end
    | _ -> failwith "too greedily, too deep"

let move_right : state_history -> state_history = fun sl ->
  match sl.right with
    | x :: xs -> {left = x :: sl.left; right = xs}
    | [] -> sl

let get_from_history : state_history -> c8_state = fun sl -> sl.left |> List.hd


let add_to_history : state_history -> c8_state -> state_history = fun sl st ->
  match sl.right with
    | [] -> {sl with left = st :: sl.left}
    | _ -> failwith "do not try time travel"







