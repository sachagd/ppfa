let key_table = Hashtbl.create 16
let has_key s = Hashtbl.mem key_table s
let set_key s = Hashtbl.replace key_table s ()
let unset_key s = Hashtbl.remove key_table s
let action_table = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let handle_input () =
  let () =
    match Gfx.poll_event () with
    | KeyDown s -> set_key s
    | KeyUp s -> unset_key s
    | Quit -> exit 0
    | MouseMove (x, y) -> Global.update (fun g -> { g with mouse_x = x; mouse_y = y })
    | _ -> ()
  in
  Hashtbl.iter (fun key action -> if has_key key then action ()) action_table
;;

let register_map km = ()

let () =
  register "z" (fun () -> Global.update (fun g -> { g with camera_y = g.camera_y - 5 }));
  register "s" (fun () -> Global.update (fun g -> { g with camera_y = g.camera_y + 5 }));
  register "q" (fun () -> Global.update (fun g -> { g with camera_x = g.camera_x - 5 }));
  register "d" (fun () -> Global.update (fun g -> { g with camera_x = g.camera_x + 5 }));
  register "n" (fun () ->
    Global.update (fun g -> { g with camera_zoom = if g.camera_zoom < 10. then g.camera_zoom +. 0.1 else g.camera_zoom}));
  register "m" (fun () ->
    Global.update (fun g -> { g with camera_zoom = if g.camera_zoom > 0.1 then g.camera_zoom -. 0.1 else g.camera_zoom}))
;;
