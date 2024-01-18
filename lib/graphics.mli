open Tsdl
open Display



val or_exit : ('a, [< `Msg of string ]) result -> 'a


val init_graphics : unit -> Sdl.renderer



val clear_graphics : Sdl.renderer -> unit


val draw_line : (c8_pixel list) -> Sdl.renderer -> int -> int -> unit

        
val draw_graphics : c8_display -> Sdl.renderer -> unit
  


