type coords =
  { x : int
  ; y : int
  }

module type Bitmap = sig
  type 'a t

  val width : 'a t -> int
  val height : 'a t -> int
  val init : int -> int -> (coords -> 'a) -> 'a t
  val fold : (coords -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val get : 'a t -> coords -> 'a
  val set : 'a t -> coords -> 'a -> unit
  val map : 'a t -> (coords -> 'a -> 'b) -> 'b t
  val of_array : 'a array array -> 'a t
end

module ArrayBitmap : Bitmap = struct
  type 'a t = 'a array array

  let width = Array.length
  let height b = Array.length b.(0)
  let init width height f = Array.init_matrix width height (fun x y -> f { x; y })

  let fold f a bm =
    Array.fold_left
      (fun (x, accl) line ->
         ( x + 1
         , Array.fold_left (fun (y, acc) c -> y + 1, f { x; y } c acc) (0, accl) line
           |> snd ))
      (0, a)
      bm
    |> snd
  ;;

  let get bm { x; y } = bm.(x).(y)
  let set bm { x; y } c = bm.(x).(y) <- c
  let map bm f = Array.mapi (fun x line -> Array.mapi (fun y c -> f { x; y } c) line) bm
  let of_array x = x
end

type directions =
  | Up
  | Down
  | Left
  | Right

module Tileset (B : Bitmap) : sig
  val extract_tiles
    :  int
    -> 'a B.t
    -> 'a array list * (directions * 'a array * 'a array, bool) Hashtbl.t
end = struct
  let get_k_tile bm pos_tiles k { x; y } =
    match Hashtbl.find_opt pos_tiles { x; y } with
    | Some t -> t
    | None ->
      Array.init (k * k) (fun i ->
        let dy = i / k in
        let dx = i mod k in
        B.get bm { x = x + dx; y = y + dy })
  ;;

  let valid_position bm { x; y } k =
    x >= 0 && y >= 0 && x + k < B.width bm && y + k < B.height bm
  ;;

  let make_get_at_opt bm k =
    let pos_tiles = Hashtbl.create 16 in
    ( pos_tiles
    , fun c ->
        if not (valid_position bm c k)
        then None
        else (
          match Hashtbl.find_opt pos_tiles c with
          | None ->
            let kt = get_k_tile bm pos_tiles k c in
            Hashtbl.add pos_tiles c kt;
            Some kt
          | Some kt -> Some kt) )
  ;;

  let move { x; y } k d =
    match d with
    | Up -> { x; y = y - k }
    | Down -> { x; y = y + k }
    | Left -> { x = x - k; y }
    | Right -> { x = x + k; y }
  ;;

  let extract_tiles k bm =
    let relations = Hashtbl.create 16 in
    let pos_tiles, get_at_opt = make_get_at_opt bm k in
    let rec aux c =
      match get_at_opt c with
      | None -> ()
      | Some at ->
        Option.iter
          (fun at_up ->
             Hashtbl.add relations (Up, at, at_up) true;
             Hashtbl.add relations (Down, at_up, at) true)
          (get_at_opt (move c k Up));
        Option.iter
          (fun at_down ->
             Hashtbl.add relations (Down, at, at_down) true;
             Hashtbl.add relations (Up, at_down, at) true)
          (get_at_opt (move c k Down));
        Option.iter
          (fun at_left ->
             Hashtbl.add relations (Left, at, at_left) true;
             Hashtbl.add relations (Right, at_left, at) true)
          (get_at_opt (move c k Left));
        Option.iter
          (fun at_right ->
             Hashtbl.add relations (Right, at, at_right) true;
             Hashtbl.add relations (Left, at_right, at) true)
          (get_at_opt (move c k Right))
    in
    for x = 0 to B.width bm - 1 do
      for y = 0 to B.height bm - 1 do
        aux { x; y }
      done
    done;
    Hashtbl.to_seq_values pos_tiles |> List.of_seq, relations
  ;;
end

module Wfc (B : Bitmap) : sig
  type 'a t

  val initialize : int -> int -> 'a list -> coords -> 'a -> 'a t
  val wfc : 'a t -> (directions * 'a * 'a, bool) Hashtbl.t -> 'a B.t Option.t
end = struct
  type 'a calculating_tile =
    | Working of 'a list
    | Done of 'a

  type 'a t = 'a calculating_tile B.t

  let initialize width height tileset coords v =
    let b = B.init width height (fun _ -> Working tileset) in
    B.set b coords (Done v);
    b
  ;;

  let move { x; y } = function
    | Up -> { x; y = y - 1 }
    | Down -> { x; y = y + 1 }
    | Left -> { x = x - 1; y }
    | Right -> { x = x + 1; y }
  ;;

  let is_valid w { x; y } = x >= 0 && x < B.width w && y >= 0 && y < B.height w

  let move_opt w position dir =
    let n = move position dir in
    if not (is_valid w n) then None else Some n
  ;;

  let get_tiles t =
    match t with
    | Working l -> l
    | Done c -> [ c ]
  ;;

  let filter_onedir w from_tiles to_tiles dir rules =
    List.filter
      (fun ft -> List.exists (fun tt -> Hashtbl.mem rules (dir, ft, tt)) to_tiles)
      from_tiles
  ;;

  let minimal_entropy w =
    B.fold
      (fun coords possibilities acc ->
         match possibilities, acc with
         | Working l, None -> Some (coords, List.length l)
         | Working l, Some (_, count) when count > List.length l ->
           Some (coords, List.length l)
         | _ -> acc)
      None
      w
  ;;

  let resolve_dir w rules position l dir =
    let to_tiles =
      Option.bind (move_opt w position dir) (fun np -> Some (B.get w np |> get_tiles))
    in
    match to_tiles with
    | None -> l
    | Some x -> filter_onedir w l x dir rules
  ;;

  let rec resolve w rules =
    let changed =
      B.fold
        (fun pos t changed ->
           match t with
           | Working l ->
             let rd = resolve_dir w rules pos in
             let ll = rd l Up in
             let ll = rd ll Down in
             let ll = rd ll Left in
             (match rd ll Right with
              | [] -> failwith "oh no"
              | [ c ] ->
                B.set w pos (Done c);
                true
              | ll ->
                if l <> ll
                then (
                  B.set w pos (Working ll);
                  true)
                else changed)
           | _ -> changed)
        false
        w
    in
    if changed then () else ()
  ;;

  let choose l =
    let n = Random.int (List.length l) in
    List.nth l n
  ;;

  let collapse w m =
    match B.get w m with
    | Working l -> B.set w m (Done (choose l))
    | Done l -> ()
  ;;

  let wfc w rules =
    let rec aux () =
      match minimal_entropy w with
      | Some (m, _) ->
        (* collapse m *)
        collapse w m;
        (* propagate *)
        resolve w rules;
        aux ()
      | None ->
        Some
          (B.map w (fun _ x ->
             match x with
             | Done c -> c
             | _ -> failwith "aie"))
    in
    try aux () with
    | _ -> None
  ;;
end
