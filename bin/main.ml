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
  Rgb24.set_strip rgb24 0 9 (200*170) (Bytes.of_string "miaou");
  (* set_strip t x_start y_start len bytes *)
  build rgb24
