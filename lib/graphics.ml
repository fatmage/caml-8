open Tsdl
open Display



let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; exit 1
  | Ok x -> x


let init_graphics () = 
  Sdl.init Sdl.Init.video |> or_exit;
  let w = Sdl.create_window ~w:960 ~h:480 "caml-8" Sdl.Window.opengl |> or_exit in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer



let clear_graphics renderer =
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let rec draw_line line renderer col row =
  match line with 
    | [] -> ()
    | p :: ps ->  match p with 
                    | PixelOn  -> let rect = Sdl.Rect.create ~x:(15 * col) ~y:(15 * row)  ~w:15 ~h:15 in 
                                  Sdl.render_fill_rect renderer (Some rect) |> or_exit;
                                  draw_line ps renderer (col + 1) row
                    | PixelOff -> draw_line ps renderer (col + 1) row

        
let draw_graphics display renderer =
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  let rec draw_lines display row =
    match display with
      | [] -> ()
      | l :: ls -> draw_line l renderer 0 row;
                   draw_lines ls (row + 1) in
  
  draw_lines display 0;
  Sdl.render_present renderer



