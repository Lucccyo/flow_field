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

let catch x y grid size_square =
  let nw_node = grid.(y/size_square).(x/size_square) in
  let ne_node = grid.(y/size_square).(x/size_square + 1) in
  let sw_node = grid.(y/size_square + 1).(x/size_square) in
  let se_node = grid.(y/size_square + 1).(x/size_square + 1) in
  nw_node, ne_node, sw_node, se_node

let rand_lst size shift max =
  let open Random in
  List.init size (fun _ -> shift + int max)

let map value min1 max1 min2 max2 = min2 +. ( max2 -. min2 ) *. (( value -. min1 ) /. ( max1 -. min1 ))

let calc_angle x y g size_square =
  let nw, ne, sw, se = catch x y g size_square in
  let nw_dist = vector_from_coordinates {x = nw.x; y = nw.y} {x; y} in
  let nw_prod = dot_product nw.gradiant_vector nw_dist in
  let ne_dist = vector_from_coordinates {x = ne.x; y = ne.y} {x; y} in
  let ne_prod = dot_product ne.gradiant_vector ne_dist in
  let sw_dist = vector_from_coordinates {x = sw.x; y = sw.y} {x; y} in
  let sw_prod = dot_product sw.gradiant_vector sw_dist in
  let se_dist = vector_from_coordinates {x = se.x; y = se.y} {x; y} in
  let se_prod = dot_product se.gradiant_vector se_dist in
  0.2



let rec iter t n cur_x cur_y color g size_square =
  let pas = 20. in
  if n = 0 then () else (
    let angle = calc_angle cur_x cur_y g size_square in
    let next_x = (cur_x + int_of_float(pas *. Float.cos(angle))) in
    let next_y = (cur_y + int_of_float(pas *. Float.sin(angle))) in
    bresenham t cur_x cur_y next_x next_y color;
    iter t (n-1) next_x next_y color g size_square)

let draw_line t start_x start_y size g size_square =
  Random.self_init ();
  let red : Color.rgba = {color = {r = ( Random.int 255 ); g = ( Random.int 255 ) ; b = 200}; alpha =  ( Random.int 255 )} in
  try
    iter t size start_x start_y red g size_square
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
      { x; y;
        gradiant_vector = vector_from_angle_len {x; y;} (Float.pi *. (Random.float 1.)) 1.;
        distance_vector = null_vector (); } ))


let () =
  Random.self_init ();
  let image_size = 2000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size black;
    let size_square = 250 in
    let g = grid image_size size_square in
    (* catch 1999 1999 g size_square; *)
    for _ = 0 to 100 do
      draw_line rgba32 ( Random.int 2000 ) ( Random.int 2000 ) ( 10 + (Random.int 100)) g size_square;
    done;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e