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
let angle_noise y x = ((float_of_int y /. float_of_int x)) *. Float.pi
let angle_random () = Random.self_init (); Float.pi *. (Random.float 1.)

let calc_angle y ymax = ((float_of_int y /. float_of_int ymax)) *. Float.pi

let rec iter t n cur_x cur_y =
  let red  : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
  let blue : Color.rgba = {color = {r = 0; g = 0; b = 255}; alpha = 255} in
  let pas = 20. in
  if n = 0 then () else (
    let angle = calc_angle cur_y 500 in
    let next_x = (cur_x + int_of_float(pas *. Float.cos(angle))) in
    let next_y = (cur_y + int_of_float(pas *. Float.sin(angle))) in
    bresenham t cur_x cur_y next_x next_y (if n mod 2 = 0 then blue else red);
    iter t (n-1) next_x next_y
  )

let draw_line t =
  let red : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
  let blue : Color.rgba = {color = {r = 0; g = 0; b = 255}; alpha = 255} in
  let start_x = 500 in
  let start_y = 300 in
  single_aliased_pixel t start_x start_y red;
  try
    iter t 30 start_x start_y
  with Images.Out_of_image -> ()

let () =
  Random.self_init ();
  let image_size = 1000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size white;
    draw_line rgba32;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e