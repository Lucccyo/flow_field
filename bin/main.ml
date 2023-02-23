let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgb24 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgb24 t)
  with Failure m -> Format.printf "Error %s@." m

let ver_strip t x len rgb =
  let dim_x, dim_y = Images.size (Rgb24 t) in
  if x > dim_x || x < 0 || x + len > dim_x then failwith "Out of image." else
  for x = x to x + len - 1 do
    for y = 0 to dim_y -1 do
      Rgb24.set t x y rgb
    done;
  done

let hor_strip t y len rgb =
  let dim_x, dim_y = Images.size (Rgb24 t) in
  if y > dim_y || y < 0 || y + len > dim_y then failwith "Out of image." else
  for y = y to y + len - 1 do
    for x = 0 to dim_x -1 do
      Rgb24.set t x y rgb
    done
  done

let tarte_linzer t x len palette =
(* ajouter le fait de decouper jusqu'en bas *)
  let x = ref x in
  let trou = !x in
  let i_color = ref 0 in
  try
    while true do
      ver_strip t !x len palette.(!i_color);
      hor_strip t !x len palette.(if !i_color + 1 < Array.length palette then !i_color + 1 else 0);
      x := !x + trou + len;
      i_color := (!i_color + 1) mod Array.length palette
    done
  with Failure _ -> ()

let rec build_palette size arr r g b: Color.rgb array =
  let open Random in
  if Array.length arr = size then arr else
  let red   = r + (let rand = 14 + (int 50) in if r + rand > 255 then 0 else rand) in
  let green = g + (let rand = 14 + (int 50) in if g + rand > 255 then 0 else rand) in
  let blue  = b + (let rand = 14 + (int 50) in if b + rand > 255 then 0 else rand) in
  build_palette size (Array.append arr [|{r; g; b}|]) red green blue

let build_palette size =
  let open Random in
  build_palette size [||] (int 256) (int 256) (int 256)

let _palette_preview t palette =
  let _dim_x, dim_y = Images.size (Rgb24 t) in
  let len = Array.length palette in
  let strip_size = dim_y / len in
  for i = 0 to len - 1 do
    hor_strip t (i*strip_size) strip_size palette.(i)
  done


let () =
  Random.self_init ();
  let rgb24 = Rgb24.create 200 200 in
  try
    tarte_linzer rgb24 8 5 (build_palette 7);
    (* palette_preview rgb24 (build_palette 7); *)
    build rgb24
  with Failure e -> Format.printf "ERROR: %s@." e
