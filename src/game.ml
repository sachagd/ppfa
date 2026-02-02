open System_defs
open Component_defs
open Ecs

type keymap =
  { move_up : string
  ; move_down : string
  ; move_left : string
  ; move_right : string
  ; attack : string
  ; pause : string
  ; start : string
  ; interact : string
  }

let init dt =
  Ecs.System.init_all dt;
  Global.update (fun g -> { g with last_update = dt });
  Some ()
;;

(* On crée une fenêtre *)

let update dt =
  let () = Input.handle_input () in
  Global.update (fun g -> { g with last_update = dt });
  Camera_system.update dt;
  None
;;

let ( let@ ) f k = f k

module Tileset = Wfc.Tileset (Wfc.ArrayBitmap)
module WfcA = Wfc.Wfc (Wfc.ArrayBitmap)

let run keymap =
  let window_spec =
    Format.sprintf "game_canvas:%dx%d:" Cst.window_width Cst.window_height
  in
  let window = Gfx.create window_spec in
  let ctx = Gfx.get_context window in
  let () = Gfx.set_context_logical_size ctx 800 600 in
  let global =
    Global.
      { window
      ; ctx
      ; last_update = 0.
      ; mouse_x = 0
      ; mouse_y = 0
      ; camera_x = 0
      ; camera_y = 0
      ; camera_zoom = 1.
      }
  in
  Global.set global;
  Input.register_map keymap;
  Random.init 15;
  let rules = Hashtbl.create 16 in
  let tiles = [ 'm'; 'g'; 'f' ] in
  let iterdir f = List.iter f Wfc.[ Up; Down; Left; Right ] in
  iterdir (fun d -> Hashtbl.add rules (d, 'm', 'm') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'm', 'g') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'g', 'm') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'g', 'f') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'f', 'g') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'g', 'g') true);
  iterdir (fun d -> Hashtbl.add rules (d, 'f', 'f') true);
  let w = WfcA.initialize 100 100 tiles Wfc.{ x = 0; y = 0 } 'm' in
  let out_bmp = Option.get (WfcA.wfc w rules) in
  Wfc.ArrayBitmap.fold
    (fun { x; y } c () ->
       Block.create
         ( x * 10
         , y * 10
         , 10
         , 10
         , match c with
           | 'm' -> Texture.black
           | 'g' -> Texture.green
           | 'f' -> Texture.red
           | _ -> Texture.white )
       |> ignore)
    ()
    out_bmp;
  let@ () = Gfx.main_loop ~limit:false init in
  let@ () = Gfx.main_loop update in
  ()
;;
