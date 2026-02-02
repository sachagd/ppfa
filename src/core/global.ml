open Component_defs

type t =
  { window : Gfx.window
  ; ctx : Gfx.context
  ; last_update : float
  ; mouse_x : int
  ; mouse_y : int
  ; camera_x : int
  ; camera_y : int
  ; camera_zoom : float
  }

let state = ref None

let get () : t =
  match !state with
  | None -> failwith "Uninitialized global state"
  | Some s -> s
;;

let set s = state := Some s

let update f =
  let g = get () in
  set (f g)
;;
