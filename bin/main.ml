let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgb24 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgb24 t)
  with Failure m -> Format.printf "Error %s@." m

let () =
  let rgb24 = Rgb24.create 200 170 in
  Rgb24.set rgb24 0 0 {r = 255; g = 0; b = 0};
  build rgb24
