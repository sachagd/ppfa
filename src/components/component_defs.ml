open Ecs

class position () =
  let r = Component.init Vector.zero in
  object
    method position = r
  end

class velocity () =
  let r = Component.init Vector.zero in
  object
    method velocity = r
  end

class box () =
  let r = Component.init Rect.{ width = 0; height = 0 } in
  object
    method box = r
  end

class texture () =
  let r = Component.init (Texture.Color (Gfx.color 0 0 0 255)) in
  object
    method texture = r
  end

type tag = No_tag

class tagged () =
  let r = Component.init No_tag in
  object
    method tag = r
  end

class type drawable = object
  inherit Entity.t
  inherit position
  inherit box
  inherit texture
end

class block () =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
  end
