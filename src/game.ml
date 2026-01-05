(* Les types des textures qu'on veut dessiner à l'écran *)
type texture =
    Color of Gfx.color
  | Image of Gfx.surface

let white = Color (Gfx.color 255 255 255 2555)


type config = {
  (* Informations des touches *)
  key_left: string;
  key_up : string;
  key_down : string;
  key_right : string;

  (* Informations de fenêtre *)
  window : Gfx.window;
  window_surface : Gfx.surface;
  ctx : Gfx.context;
}


(* On crée une fenêtre *)

let draw_rect config texture x y w h = failwith "todo"

let update cfg dt = failwith "todo"

let run keys = failwith "todo"
