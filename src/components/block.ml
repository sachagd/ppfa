open Ecs
open Component_defs
open System_defs

let create (x, y, width, height, txt) =
  let e = new block () in
  e#texture#set txt;
  e#position#set Vector.{ x = float x; y = float y };
  e#box#set Rect.{ width; height };
  Camera_system.(register (e :> t));
  e
;;
