open Tsdl
open Display
open State



val or_exit : ('a, [< `Msg of string ]) result -> 'a


val init_graphics : unit ->  Sdl.window * Sdl.renderer



val clear_graphics : Sdl.renderer -> unit


val draw_line : (c8_pixel list) -> Sdl.renderer -> int -> int -> unit

        
val draw_graphics : c8_display -> Sdl.renderer -> unit

val draw_debug_info : c8_state -> Sdl.renderer -> unit
  
val close_graphics : Sdl.window -> Sdl.renderer -> unit


