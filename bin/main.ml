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

let grid t =
  let xmax, ymax = Images.size (Rgba32 t) in
  let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 50} in
  let green : Color.rgba = {color = {r = 0; g = 255; b = 0}; alpha = 255} in
  let space = 40 in
  let g = Array.init (ymax/space) (fun y ->
    Array.init (xmax/space) (fun x -> 0 )) in
  for y_grid = 0 to Array.length g do
    for x_grid = 0 to Array.length g.(0) do
      try
        let x = x_grid * space in
        let y = y_grid * space in
        single_aliased_pixel t (x + 20) (y + 20) green;
        bresenham t x y (x + 40) y black;
        bresenham t x y x (y + 40) black;
        (* bresenham t x y (x * space) (y * space) black; *)
        (* center *)
      with Images.Out_of_image -> ()
    done
  done


let () =
  Random.self_init ();
  let image_size = 1000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size white;
    let g = grid rgba32 in
    (* let vecs = draw_vecs_field rgba32 in *)
    (* curve rgba32 vecs red; *)
    (* bresenham rgba32 0 0 30 0 red;
    bresenham rgba32 0 0 0 30 red; *)
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e