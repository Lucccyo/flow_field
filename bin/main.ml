open Data.Bezier

type grid_node = {x : int; y : int; angle : float; len : int}

let get_values t = t.x, t.y, t.angle, t.len

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
    iter t (n-1) next_x next_y color
  )

let draw_line t start_x start_y size =
  Random.self_init ();
  let red : Color.rgba = {color = {r = ( Random.int 255 ); g = ( Random.int 255 ) ; b = 200}; alpha =  ( Random.int 255 )} in
  try
    iter t size start_x start_y red
  with Images.Out_of_image -> ()

let print_value g =
  for y = 0 to Array.length g - 1 do
    for x = 0 to Array.length g.(0) - 1 do
      let x, y, angle, len = get_values g.(y).(x) in
      Format.printf "(%d;%d = %f, %d)" x y angle len
    done;
    Format.printf "\n"
  done

let grid dim size_square =
  Array.init ((dim / size_square) + 1)
    (fun y -> Array.init ((dim / size_square) + 1)
    (fun x ->
      {x = x * size_square; y =  y * size_square; angle = Float.pi *. (Random.float 1.); len = 1}))

let catch x y grid size_square =
  let nw_x, nw_y, nw_angle, nw_len = get_values grid.(y/size_square).(x/size_square) in
  let ne_x, ne_y, ne_angle, ne_len = get_values grid.(y/size_square).(x/size_square + 1) in
  let sw_x, sw_y, sw_angle, sw_len = get_values grid.(y/size_square + 1).(x/size_square) in
  let se_x, se_y, se_angle, se_len = get_values grid.(y/size_square + 1).(x/size_square + 1) in
  Format.printf "\nDans le carré:\tnw:(%d;%d = %f, %d)\tne:(%d;%d = %f, %d)\tsw:(%d;%d = %f, %d)\tse:(%d;%d = %f, %d)\n"
  nw_x nw_y nw_angle nw_len ne_x ne_y ne_angle ne_len sw_x sw_y sw_angle sw_len se_x se_y se_angle se_len;
  ()

let () =
  Random.self_init ();
  let image_size = 2000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in

    let size_square = 250 in
    let grid = grid image_size size_square in
    print_value grid;
    catch 1900 1900 grid size_square;

    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e