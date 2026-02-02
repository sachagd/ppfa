open Vector

type t =
  { width : int
  ; height : int
  }

let mdiff v1 r1 v2 r2 =
  ( Vector.{ x = v2.x -. v1.x -. float r1.width; y = v2.y -. v1.y -. float r1.height }
  , { width = r1.width + r2.width; height = r1.height + r2.height } )
;;

let has_origin { x; y } { width; height } =
  x < 0. && x +. float width > 0. && y < 0. && y +. float height > 0.
;;

let min_norm v1 v2 = if Vector.norm v1 <= Vector.norm v2 then v1 else v2

let intersect v1 r1 v2 r2 =
  let s_pos, s_rect = mdiff v1 r1 v2 r2 in
  has_origin s_pos s_rect
;;
