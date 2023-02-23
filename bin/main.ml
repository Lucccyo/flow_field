let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgb24 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgb24 t)
  with Failure m -> Format.printf "Error %s@." m

(* type rgb = {
  r: int;
  g: int;
  b: int;
} *)

let () =
  let palette : Color.rgb array =
    (* from https://colorhunt.co/palette/7286d38ea7e9e5e0fffff2f2 *)
    [|{r = 114; g = 134; b = 211};
      {r = 142; g = 167; b = 233};
      {r = 229; g = 224; b = 255};
      {r = 255; g = 242; b = 242}|] in
  let rgb24 = Rgb24.create 200 170 in
   for x = 0 to 199 do
    for y = 0 to 169 do
      Rgb24.set rgb24 x y palette.(if (x*y) mod 2 = 0 then 0 else 1)
    done;
   done;
  (* Rgb24.set_strip rgb24 0 9 (200*170) (Bytes.of_string "red"); *)
  (* set_strip t x_start y_start len bytes *)
  build rgb24
