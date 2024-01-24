open Tsdl
open Display
open State
open Inttypes


let or_exit : ('a, [< `Msg of string ]) result -> 'a = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let or_print : ('a, [< `Msg of string ]) result -> 'a = function
  | Error (`Msg e) -> Sdl.log "%s" e; Printf.printf "%s" e; exit 1
  | Ok x -> x

let debug_bg = Sdl.load_bmp "assets/register_info.bmp" |> or_print 

let zero_char = Sdl.load_bmp "assets/0.bmp" |> or_print
let one_char = Sdl.load_bmp "assets/1.bmp" |> or_print
let two_char = Sdl.load_bmp "assets/2.bmp" |> or_print
let three_char = Sdl.load_bmp "assets/3.bmp" |> or_print
let four_char = Sdl.load_bmp "assets/4.bmp" |> or_print
let five_char = Sdl.load_bmp "assets/5.bmp" |> or_print
let six_char = Sdl.load_bmp "assets/6.bmp" |> or_print
let seven_char = Sdl.load_bmp "assets/7.bmp" |> or_print
let eight_char = Sdl.load_bmp "assets/8.bmp" |> or_print
let nine_char = Sdl.load_bmp "assets/9.bmp" |> or_print
let a_char = Sdl.load_bmp "assets/A.bmp" |> or_print
let b_char = Sdl.load_bmp "assets/B.bmp" |> or_print
let c_char = Sdl.load_bmp "assets/C.bmp" |> or_print
let d_char = Sdl.load_bmp "assets/D.bmp" |> or_print
let e_char = Sdl.load_bmp "assets/E.bmp" |> or_print
let f_char = Sdl.load_bmp "assets/F.bmp" |> or_print
let x_char = Sdl.load_bmp "assets/x.bmp" |> or_print


let letter_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:18 ~h:27
let letter_w = 18
let letter_h = 27
let col_1_start = 1020
let row_start = 7
let col_2_start = 1165



let init_graphics : unit -> Sdl.renderer = fun _ -> 
  Sdl.init Sdl.Init.video |> or_exit;
  let w = Sdl.create_window ~w:1260 ~h:480 "caml-8" Sdl.Window.opengl |> or_exit in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer



let clear_graphics : Sdl.renderer -> unit = fun renderer ->
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let rec draw_line : (c8_pixel list) -> Sdl.renderer -> int -> int -> unit = 
  fun line renderer col row ->
  match line with 
    | [] -> ()
    | p :: ps ->  match p with 
                    | PixelOn  -> let rect = Sdl.Rect.create ~x:(15 * col) ~y:(15 * row)  ~w:15 ~h:15 in 
                                  Sdl.render_fill_rect renderer (Some rect) |> or_exit;
                                  draw_line ps renderer (col + 1) row
                    | PixelOff -> draw_line ps renderer (col + 1) row

        
let draw_graphics : c8_display -> Sdl.renderer -> unit = fun display renderer ->
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  let rec draw_lines display row =
    match display with
      | [] -> ()
      | l :: ls -> draw_line l renderer 0 row;
                   draw_lines ls (row + 1) in
  
  draw_lines display 0;
  Sdl.render_present renderer


let draw_letter : char -> int -> int -> Sdl.renderer -> unit = fun ch x y renderer ->
  let texture = begin match ch with 
                        | '0' -> Sdl.create_texture_from_surface renderer zero_char |> or_print
                        | '1' -> Sdl.create_texture_from_surface renderer one_char |> or_print
                        | '2' -> Sdl.create_texture_from_surface renderer two_char |> or_print
                        | '3' -> Sdl.create_texture_from_surface renderer three_char |> or_print
                        | '4' -> Sdl.create_texture_from_surface renderer four_char |> or_print
                        | '5' -> Sdl.create_texture_from_surface renderer five_char |> or_print
                        | '6' -> Sdl.create_texture_from_surface renderer six_char |> or_print
                        | '7' -> Sdl.create_texture_from_surface renderer seven_char |> or_print
                        | '8' -> Sdl.create_texture_from_surface renderer eight_char |> or_print
                        | '9' -> Sdl.create_texture_from_surface renderer nine_char |> or_print
                        | 'a' -> Sdl.create_texture_from_surface renderer a_char |> or_print
                        | 'b' -> Sdl.create_texture_from_surface renderer b_char |> or_print
                        | 'c' -> Sdl.create_texture_from_surface renderer c_char |> or_print
                        | 'd' -> Sdl.create_texture_from_surface renderer d_char |> or_print
                        | 'e' -> Sdl.create_texture_from_surface renderer e_char |> or_print
                        | 'f' -> Sdl.create_texture_from_surface renderer f_char |> or_print
                        | 'x' -> Sdl.create_texture_from_surface renderer x_char |> or_print
                        | _ -> failwith "unknown letter"; end in
  let target = Sdl.Rect.create ~x:x ~y:y ~w:letter_w ~h:letter_h in
  Sdl.render_copy ~src:letter_rect ~dst:target renderer texture |> or_print;
  Sdl.destroy_texture texture

let draw_string : string -> int -> int -> Sdl.renderer -> unit = fun str x y renderer ->
  String.iteri (fun i ch -> draw_letter ch (x + (i * letter_w)) y renderer) str


let draw_registers : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  let rec hlp = fun reg -> match (U8.to_int reg) with
    | 0xF -> draw_string ((get_reg state reg) |> U8.to_hexstring) col_1_start (0xF * letter_h + row_start) renderer
    | x -> draw_string  ((get_reg state reg) |> U8.to_hexstring) col_1_start (x * letter_h + row_start) renderer;
           hlp (U8.succ reg); in
  hlp U8.zero

let draw_PC : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  draw_string (state |> get_pc |> U16.to_hexstring) col_2_start row_start renderer

let draw_opcode : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  let opcode = fetch_opcode state in
  let op_str_raw = U16.to_hexstring opcode in
  let opcode_str = String.sub op_str_raw 2 ((String.length op_str_raw) - 2) in
  draw_string opcode_str col_2_start (row_start + 1 * letter_h) renderer

let draw_IR : c8_state -> Sdl.renderer -> unit  = fun state renderer ->
  draw_string (state |> get_ir |> U16.to_hexstring) col_2_start (row_start + 6 * letter_h) renderer;
  draw_string (U8.to_hexstring (fetch_mem state (get_ir state))) col_2_start (row_start + 7 * letter_h) renderer

let draw_timers : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  draw_string (state |> get_dt |> U8.to_hexstring) col_2_start (row_start + 3 * letter_h) renderer;
  draw_string (state |> get_st |> U8.to_hexstring) col_2_start (row_start + 4 * letter_h) renderer

let draw_stack : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  draw_string (state |> stack_depth |> U8.to_hexstring) col_2_start (row_start + 9 * letter_h) renderer;
  draw_string (state |> hd_stack |> U16.to_hexstring) col_2_start (row_start + 10 * letter_h) renderer


let draw_state : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  draw_registers state renderer;
  draw_PC state renderer;
  draw_opcode state renderer;
  draw_IR state renderer;
  draw_timers state renderer;
  draw_stack state renderer



  

let draw_debug_info : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:300 ~h:480 in
  let target = Sdl.Rect.create ~x:960 ~y:0 ~w:300 ~h:480 in
  let texture = Sdl.create_texture_from_surface renderer debug_bg |> or_print in
  Sdl.render_copy ~src:src ~dst:target renderer texture |> or_print;
  draw_state state renderer;
  Sdl.render_present renderer;
  Sdl.destroy_texture texture



