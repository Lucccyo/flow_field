open Data.Bezier
open Data.Vector

type node = {
  x: int;
  y: int;
  gradiant_vector: vector;
  distance_vector: vector;
}

let pp_node node =
  Format.printf "{x = %d\ny = %d\ngradiant_vector = {dir = %f ; norm = %f ; c = (%d;%d)}\ndistance_vector = {dir = %f ; norm = %f ; c = (%d;%d)}}\n"
    node.x node.y node.gradiant_vector.dir node.gradiant_vector.norm node.gradiant_vector.c.x node.gradiant_vector.c.y
      node.distance_vector.dir node.distance_vector.norm node.distance_vector.c.x node.distance_vector.c.y

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

let map value min1 max1 min2 max2 = min2 +. ( max2 -. min2 ) *. (( value -. min1 ) /. ( max1 -. min1 ))

let calc_angle x y xmax ymax =
  (* let noise = (float_of_int y /. float_of_int xmax) +. Float.pi /. 5. in *)
  let noise = Float.atan2((float_of_int y) -. 1000.) (float_of_int x -. 1000.) +. (Float.pi /. 6.) in
  (* let noise = Float.pi *. (Random.float 100.) in *)
  let angle = map noise (Float.pi *. -1.) (Float.pi) 0.0 (Float.pi *. 2.0) in
  (* Format.printf "noise = %f\n" noise; *)
  angle

let rec iter t n cur_x cur_y color =
  let pas = 20. in
  if n = 0 then () else (
    let angle = calc_angle cur_x cur_y 2000 2000 in
    let next_x = (cur_x + int_of_float(pas *. Float.cos(angle))) in
    let next_y = (cur_y + int_of_float(pas *. Float.sin(angle))) in
    bresenham t cur_x cur_y next_x next_y color;
    iter t (n-1) next_x next_y color )

let draw_line t start_x start_y size =
  Random.self_init ();
  let red : Color.rgba = {color = {r = ( Random.int 255 ); g = ( Random.int 255 ) ; b = 200}; alpha =  ( Random.int 255 )} in
  try
    iter t size start_x start_y red
  with Images.Out_of_image -> ()

let print_grid g =
  for y = 0 to Array.length g - 1 do
    for x = 0 to Array.length g.(0) - 1 do
      pp_node g.(x).(y)
    done;
    Format.printf "\n"
  done

let grid dim_image size_square =
  Array.init ((dim_image / size_square) + 1)
    (fun y_grid -> Array.init ((dim_image / size_square) + 1)
    (fun x_grid ->
      let x = x_grid * size_square in
      let y = y_grid * size_square in
      { x = x; y = x;
        gradiant_vector = vector_from_angle_len {x; y;} (Float.pi *. (Random.float 1.)) 1.;
        distance_vector = null_vector (); } ))

(* let catch x y grid size_square =
  let nw_x, nw_y, nw_angle, nw_len = get_values grid.(y/size_square).(x/size_square) in
  let ne_x, ne_y, ne_angle, ne_len = get_values grid.(y/size_square).(x/size_square + 1) in
  let sw_x, sw_y, sw_angle, sw_len = get_values grid.(y/size_square + 1).(x/size_square) in
  let se_x, se_y, se_angle, se_len = get_values grid.(y/size_square + 1).(x/size_square + 1) in
  Format.printf "\nDans le carré:\tnw:(%d;%d = %f, %d)\tne:(%d;%d = %f, %d)\tsw:(%d;%d = %f, %d)\tse:(%d;%d = %f, %d)\n"
  nw_x nw_y nw_angle nw_len ne_x ne_y ne_angle ne_len sw_x sw_y sw_angle sw_len se_x se_y se_angle se_len;
  (* () *) *)

let () =
  Random.self_init ();
  let image_size = 2000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size black;
    let g = grid image_size 250 in
    print_grid g;
    (* for _ = 0 to 600 do
      draw_line rgba32 ( Random.int 2000 ) ( Random.int 2000 ) ( 10 + (Random.int 100));
    done; *)
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e