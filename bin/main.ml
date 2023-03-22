open Data.Bezier

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

let angle_default () = Float.pi *. 0.25
let angle_random  () = Float.pi *. (Random.float 1.)
let angle_noise y y_max = ((float_of_int y /. float_of_int y_max)) *. Float.pi




let vector_field dim_x dim_y space shift =
  Array.init ((dim_y / space) - (shift * 1))
    (fun y -> Array.init ((dim_x / space) - (shift * 1))
      (fun _ -> angle_noise y (dim_y / space) ))


  (* for (column in num_columns) {
    for (row in num_rows) {
      (* Processing's noise() works best when the step between *)
      (* points is approximately 0.005, so scale down to that *)
      scaled_x = column * 0.005
      scaled_y = row * 0.005
      (* get our noise value, between 0.0 and 1.0 *)
      noise_val = noise(scaled_x, scaled_y)
      (* translate the noise value to an angle (betwen 0 and 2 * PI) *)
      angle = map(noise_val, 0.0, 1.0, 0.0, PI * 2.0)
      grid[column][row] = angle   } } *)

let end_coord dist angle x y space =
  int_of_float (dist *. Float.cos(angle)) + (x * space),
  int_of_float (dist *. Float.sin(angle)) + (y * space)

let draw_vecs_field t =
  let space = 30 in
  let dist = 20. in
  let shift = 1 in
  let dim_x, dim_y = Images.size (Rgba32 t) in
  let vecs = vector_field dim_x dim_y space shift in
  Array.iteri (fun y array_x -> Array.iteri (fun x e ->
    let x, y = x + shift, y + shift in
    let x_end, y_end = end_coord dist e x y space in
    bresenham t (x * space) (y * space) x_end y_end {color = {r = 0; g = 45; b = 200}; alpha = 255};
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