open Data.Bezier

(* type angle = { mutable angle : float; } *)

let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgba32 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgba32 t)
  with Failure m -> Format.printf "Error %s@." m

let rand_lst size shift max =
  let open Random in
  List.init size (fun _ -> shift + int max)

let def_angle = Float.pi *. 0.25

let rand_angle = Float.pi *. (Random.float 1.)

let vector_field dim_x dim_y space = Array.init ((dim_y / space) - 10) (fun _ -> Array.init ((dim_x / space) - 10) (fun _ -> def_angle))

let end_coord dist angle x y space =
  int_of_float (dist *. Float.cos(angle)) + (x * space),
  int_of_float (dist *. Float.cos(angle)) + (y * space)

let draw_vecs_field t =
  let space = 30 in
  let dist = 25. in
  let dim_x, dim_y = Images.size (Rgba32 t) in
  let vecs = vector_field dim_x dim_y space in
  Array.iteri (fun y array_x -> Array.iteri (fun x e ->
    let x, y = x + 5, y + 5 in
    let x_end, y_end = end_coord dist e x y space in
    bresenham t (x * space) (y * space) x_end y_end {color = {r = 0; g = 45; b = 200}; alpha = 255};
    single_aliased_pixel t (x * space ) (y * space) {color = {r = 255; g = 0; b = 0}; alpha = 255}
  ) array_x) vecs; ()

let () =
  Random.self_init ();
  let image_size = 1000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 50} in
    hor_strip rgba32 0 image_size white;
    draw_vecs_field rgba32;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e