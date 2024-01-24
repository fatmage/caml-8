open Inttypes

type c8_pixel = PixelOn | PixelOff
type c8_display = (c8_pixel list) list

val empty_display : c8_display 


val update_line : c8_display -> (c8_pixel list) -> uint8 -> c8_display
val byte_to_line : uint8 -> uint8 -> c8_pixel list
val draw_sprite_display : c8_display -> ((c8_pixel list) list) -> uint8 -> c8_display
val check_for_flag : c8_display -> c8_display -> uint8
