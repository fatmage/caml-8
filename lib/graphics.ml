open Tsdl
open Display
open State




let or_exit : ('a, [< `Msg of string ]) result -> 'a = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x

let or_print : ('a, [< `Msg of string ]) result -> 'a = function
  | Error (`Msg e) -> Sdl.log "%s" e; Printf.printf "%s" e; exit 1
  | Ok x -> x

let debug_bg = Sdl.load_bmp "assets/register_info.bmp" |> or_print 

let zero_char = Sdl.load_bmp "assets/0.bmp" |> or_print

let letter_rect = Sdl.Rect.create ~x:0 ~y:0 ~w:20 ~h:30


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
  let texture = Sdl.create_texture_from_surface renderer zero_char |> or_print in
  let target = Sdl.Rect.create ~x:x ~y:y ~w:20 ~h:30 in
  Sdl.render_copy ~src:letter_rect ~dst:target renderer texture |> or_print;
  Sdl.destroy_texture texture
    
  

let draw_debug_info : c8_state -> Sdl.renderer -> unit = fun state renderer ->
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:300 ~h:480 in
  let target = Sdl.Rect.create ~x:960 ~y:0 ~w:300 ~h:480 in
  let texture = Sdl.create_texture_from_surface renderer debug_bg |> or_print in
  Sdl.render_copy ~src:src ~dst:target renderer texture |> or_print;
  draw_letter '0' 1020 8 renderer;
  Sdl.render_present renderer;
  Sdl.destroy_texture texture;



