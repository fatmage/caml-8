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

let xor_pixel_line : (c8_pixel list) -> (c8_pixel list) -> (c8_pixel list) = 
  fun line1 line2 ->
    List.map2 (fun p1 p2 -> match p1, p2 with 
                              | PixelOff, PixelOn  -> PixelOn
                              | PixelOn,  PixelOff -> PixelOn
                              | PixelOff, PixelOff -> PixelOff
                              | PixelOn,  PixelOn  -> PixelOff) line1 line2
                     


let rec update_line : c8_display -> (c8_pixel list) -> uint8 -> c8_display = 
  fun disp line row -> 
    if U8.eq row U8.zero then 
      match disp with 
        | x :: xs -> (xor_pixel_line x line) :: xs
        | _ -> failwith "???"
    else
      match disp with 
        | x :: xs -> x :: (update_line xs line (U8.pred row))
        | _ -> failwith "???"


let rec take_pixels : (c8_pixel list) -> int -> (c8_pixel list) =
  fun pixels num -> match pixels, num with 
    | [], _ -> []
    | _, 0 -> []
    | p::ps, x -> p :: (take_pixels ps (x - 1))

let rec byte_to_arr : uint8 -> int -> (c8_pixel list) -> (c8_pixel list) = 
  fun byte num acc  -> match num with
    | 0 -> acc
    | x -> byte_to_arr (U8.shr byte U8.one) (num - 1) (if (U8.rem byte U8.two) == U8.one then PixelOn :: acc else PixelOff :: acc)

let byte_to_line : uint8 -> uint8 -> c8_pixel list = 
  fun byte col -> 
  let rec off_sequence num = match num with
    | 0 -> []
    | x -> PixelOff :: off_sequence (num - 1) in 
  let right_len = 64 - ((U8.to_int col) + 8) in 
  let right_seq = if right_len > 0 then off_sequence right_len else [] in
  let left_seq = off_sequence (U8.to_int col) in
  let middle = take_pixels (byte_to_arr byte 8 []) (if 64 - (U8.to_int col) > 0 then 64 - (U8.to_int col) else 0) in
  left_seq @ middle @ right_seq 


let rec draw_sprite_display : c8_display -> ((c8_pixel list) list) -> uint8 -> c8_display =
  fun disp sprite row -> match disp, (U8.to_int row) with 
    | [], _ -> []
    | c :: cs, 0 -> begin match sprite with
      | s :: ss -> (xor_pixel_line c s) :: (draw_sprite_display cs ss row)
      | [] -> c :: cs
      end
    | c :: cs, x -> c :: (draw_sprite_display cs sprite (U8.pred row))

