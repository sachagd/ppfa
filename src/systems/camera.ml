open Ecs
open Component_defs

type t = drawable

let init _ = ()
let white = Gfx.color 255 255 255 255

let zoom_centered pos box zoom =
    (* move pos and resize box by zoom s.t. the box centre position stays the same *)
    let resized_box = Rect.{width = int_of_float (float box.width *. zoom); height = int_of_float (float box.height *. zoom)} in
    let centered_pos = Vector.add pos (Vector.mult ((1. -. zoom)/.2.) (Vector.{x = float box.width; y = float box.height})) in
    centered_pos, resized_box

let update _dt el =
  let Global.{ window; ctx; camera_x; camera_y; camera_zoom; _ } = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  Gfx.set_color ctx white;
  Gfx.fill_rect ctx surface 0 0 ww wh;
  let centered_camera_pos, resized_camera_box = zoom_centered (Vector.{x = float camera_x; y = float camera_y}) (Rect.{width = ww; height = wh}) (1./.camera_zoom) in
  Seq.iter
    (fun (e : t) ->
       let real_pos = e#position#get in
       let real_box = e#box#get in
       (* draw *)
       let resized_box = Rect.{width = int_of_float (float real_box.width *. camera_zoom); height = int_of_float (float real_box.height *. camera_zoom)} in
       let shifted_pos = Vector.sub real_pos centered_camera_pos in
       let resized_pos = Vector.mult camera_zoom shifted_pos in
       if Rect.intersect centered_camera_pos resized_camera_box real_pos resized_box then
           let txt = e#texture#get in
       Texture.draw ctx surface resized_pos resized_box txt)
    el;
  Gfx.commit ctx
;;
