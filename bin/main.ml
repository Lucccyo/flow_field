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

let angle_default () =
  let a = Float.pi *. 0.10 in
  Format.printf "%f\n" a;
  a

let angle_random  () = Float.pi *. (Random.float 1.)
let angle_noise x y ymax xmax =
  let res = (float_of_int y /. float_of_int ymax) *. Float.pi *. (float_of_int x /. float_of_int xmax) in
  Format.printf "%d %d :: %f\n" x y res;
  res

let vector_field dim_x dim_y space shift =
  Array.init ((dim_y / space) - (shift * 1))
    (fun y -> Array.init ((dim_x / space) - (shift * 1))
    (* (fun _ -> angle_noise y (dim_y / space) )) *)
    (fun x -> angle_noise x y (dim_y / space) (dim_x / space)))




let end_coord dist angle x y space =
  int_of_float (dist *. Float.cos(angle)) + (x * space),
  int_of_float (dist *. Float.sin(angle)) + (y * space)

(* let draw_vecs_field t =
  let space = 30 in
  let dist = 20. in
  let shift = 0 in
  let dim_x, dim_y = Images.size (Rgba32 t) in
  let vecs = vector_field dim_x dim_y space shift in
  Array.iteri (fun y array_x -> Array.iteri (fun x e ->
    let x, y = x + shift, y + shift in
    let x_end, y_end = end_coord dist e x y space in
    bresenham t (x * space) (y * space) x_end y_end {color = {r = 0; g = 45; b = 200}; alpha = 255};
  ) array_x) vecs; vecs *)


  (* starting point x = 500 y = 100

  (* begin_curve() *)
  for (n in [0..num_steps]) {
      draw_vertex(x, y)
      x_offset = x - left_x     y_offset = y - top_y
      column_index = int(x_offset / resolution)     row_index = int(y_offset / resolution)
      (* // NOTE: normally you want to check the bounds here *)
      grid_angle = grid[column_index][row_index]
      x_step = step_length * cos(grid_angle)
      y_step = step_length * sin(grid_angle)
      x = x + x_step     y = y + y_step } *)
  (* end_curve() *)

(* let curve t vecs =
  let x = 500. in
  let y = 500. in
  let resolution = 20. in
  let step_length = 0.5 in
  let rec loop n x y =
    if n <= 0 then ()
    else begin
    (* draw *)
      try
        single_aliased_pixel t (int_of_float x) (int_of_float y) {color ={r = 255; g = 0; b = 0}; alpha = 50};
        (* bresenham t (int_of_float x) (int_of_float y) (int_of_float nextx) (int_of_float nexty) {color ={r = 255; g = 0; b = 0}; alpha = 50}; *)
        (* Format.printf "x = %f y = %f@." x y; *)
        let x_offset = x -. 20. in
        let y_offset = x -. 20. in
        let column_index = int_of_float (x_offset /. resolution) in
        let row_index = int_of_float (y_offset /. resolution) in
        (* Format.printf "%d %d@." column_index row_index; *)
        (* Format.printf "%d %d@." (Array.length vecs) (Array.length vecs.(0)); *)

        let angle = vecs.(column_index).(row_index) in
        loop (n-1) (x +. step_length *. Float.cos(angle)) (y +. step_length *. Float.sin(angle))
      with Invalid_argument _-> Format.printf "erro@."
      end in
  loop 900 x y *)


(* let curve t vecs color =
  let x_grid = 8 in
  let y_grid = 31 in
  let space = 10 in
  let x = x_grid * space in
  let y = y_grid * space in
  let depx = 0 in
  let depy = 0 in
  let pas = 30. in
  (* Format.printf "%f\n" vecs.(16).(31) *)
  let teta = vecs.(x_grid).(y_grid) in
  Format.printf "teta = %f\n" teta;
  let ux = Float.cos(teta) in
  let uy = Float.sin(teta) in
  let x1 = (int_of_float (pas *. ux)) * space in
  let y1 = (int_of_float (pas *. uy)) * space in
  Format.printf "(%d;%d) -> (%d;%d)\n" x y x1 y1;
  bresenham t x y x1 y1 color;
  () *)

let () =
  Random.self_init ();
  let image_size = 1000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size white;
    let vecs = draw_vecs_field rgba32 in
    curve rgba32 vecs red;
    bresenham rgba32 0 0 30 0 red;
    bresenham rgba32 0 0 0 30 red;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e