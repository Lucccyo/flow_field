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

let () =
  Random.self_init ();
  let image_size = 1000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 50} in
    hor_strip rgba32 0 image_size white;
    ribbon rgba32
      (curve_points rgba32 (rand_lst 5 20 950) (rand_lst 5 20 950))
      (curve_points rgba32 (rand_lst 5 20 950) (rand_lst 5 20 950))
      (curve_points rgba32 (rand_lst 5 20 950) (rand_lst 5 20 950)) red;
    (* done; *)
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e