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

(* let angle_default () = Float.pi *. 0.25 *)
(* let angle_noise y x = ((float_of_int y /. float_of_int x)) *. Float.pi *)
(* let angle_random () = Random.self_init (); Float.pi *. (Random.float 1.) *)

(* let calc_angle x y = (float_of_int x +. float_of_int y) *. Float.pi -. Float.pi /. 6. *)

(* let calc_angle x y xmax ymax = Float.pi *. (float_of_int y /. float_of_int ymax /. (float_of_int x +. float_of_int y) /. 36.) *)

(* let calc_angle x y xmax ymax = Float.pi *. (float_of_int y /. float_of_int ymax +. (float_of_int x +. float_of_int y) /. 36. +. ( 1. /. 2. ) *. (float_of_int x /. float_of_int xmax)) *)

let map value cmin cmax nmin nmax = nmin +. ( nmax -. nmin ) *. (( value -. cmin ) /. ( cmax -. cmin ))
let calc_angle x y xmax ymax =
  let noise = Float.cos(float_of_int y /. float_of_int xmax) +. Float.pi /. 2.0 in
  (* let noise = Float.pi *. (Random.float 100.) in *)
  map noise 0.0 1.0 0.0 (Float.pi *. 4.0)


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
  let red : Color.rgba = {color = {r = 255; g = ( Random.int 255 ) ; b = ( Random.int 255 )}; alpha = 255} in
  try
    iter t size start_x start_y red
  with Images.Out_of_image -> ()

let () =
  Random.self_init ();
  let image_size = 2000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size black;
    for _ = 0 to 600 do
      draw_line rgba32 ( Random.int 2000 ) ( Random.int 2000 ) ( 30 + (Random.int 500));
    done;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e