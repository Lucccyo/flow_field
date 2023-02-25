let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgba32 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgba32 t)
  with Failure m -> Format.printf "Error %s@." m

let _ver_line t x y len rgb =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if x > dim_x || x < 0 || y > dim_y || y < 0 || y + len > dim_y then failwith "Out of image." else
  for y = y to y + len - 1 do
      Rgba32.set t x y rgb
  done

let _hor_line t x y len rgba =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if y > dim_y || y < 0 || x > dim_x || x < 0 || x + len > dim_x then failwith "Out of image." else
  for x = x to x + len - 1 do
    Rgba32.set t x y rgba
  done


let ver_strip t x len rgb =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if x > dim_x || x < 0 || x + len > dim_x then failwith "Out of image." else
  for x = x to x + len - 1 do
    for y = 0 to dim_y -1 do
      Rgba32.set t x y rgb
    done;
  done

let hor_strip t y len rgb =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if y > dim_y || y < 0 || y + len > dim_y then failwith "Out of image." else
  for y = y to y + len - 1 do
    for x = 0 to dim_x -1 do
      Rgba32.set t x y rgb
    done
  done

let _tarte_linzer t ofs bs palette =
  let c = ref ofs in
  let i_color = ref 0 in
  try
    while true do
      ver_strip t !c bs palette.(!i_color);
      hor_strip t !c bs palette.(if !i_color + 1 < Array.length palette then !i_color + 1 else 0);
      c := !c + bs + ofs;
      i_color := (!i_color + 1) mod Array.length palette
    done
  with Failure _ -> ()

let rec build_palette size arr r g b: Color.rgba array =
  let ofs = 50 / size in
  let open Random in
  if Array.length arr = size then arr else
  let red   = r + (let rand = ofs + (int 50) in if r + rand > 255 then 0 else rand) in
  let green = g + (let rand = ofs + (int 50) in if g + rand > 255 then 0 else rand) in
  let blue  = b + (let rand = ofs + (int 50) in if b + rand > 255 then 0 else rand) in
  build_palette size (Array.append arr [|{color = {r; g; b}; alpha = 255}|]) red green blue

let _build_palette size =
  let open Random in
  let ofs = 100 / size in
  build_palette size [||] (int ofs) (int ofs) (int ofs)

let _palette_preview t palette =
  let _dim_x, dim_y = Images.size (Rgba32 t) in
  let len = Array.length palette in
  let strip_size = dim_y / len in
  let res = dim_y mod len in
  for i = 0 to len - 1 do
    hor_strip t (i*strip_size) strip_size palette.(i)
  done;
  hor_strip t (strip_size * len) res palette.(len-1)


let _draw_line t xs ys xe ye =
  (* let x = ref xs in *)
    let red : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
  for x = xs to xe do
    let ideal_y = ((ye - ys)/(xe - xs)) * (x - xs) + ys in
    Rgba32.set t x ideal_y red
  done
let single_aliased_pixel t x y color =
  (* cache cache d'erreur *)
  let _lowpacity      : Color.rgba = {color with alpha = 1/15} in
  let lowpacity      : Color.rgba = {color with alpha = color.alpha /10} in
  let middle_opacity : Color.rgba = {color with alpha = color.alpha /4} in
  let top_opacity : Color.rgba = color in
  Rgba32.set t (x - 1) (y - 1) lowpacity;
  Rgba32.set t x (y - 1) middle_opacity;
  Rgba32.set t (x + 1) (y - 1) lowpacity;
  Rgba32.set t (x - 1) y middle_opacity;
  Rgba32.set t x y top_opacity;
  Rgba32.set t (x + 1) y middle_opacity;
  Rgba32.set t (x - 1) (y + 1) lowpacity;
  Rgba32.set t x (y + 1) middle_opacity;
  Rgba32.set t (x + 1) (y + 1) lowpacity

let bresenham t x0 y0 x1 y1 color =
  let dx = float_of_int (Int.abs(x1 - x0)) in
  let dy = float_of_int (Int.abs(y1 - y0)) in
  let sr = if dx > dy then dx else dy in
  let lr = if dx > dy then dy else dx in
  let err = ref (sr /. 2.) in
  let curr_lr = ref (if dx > dy then y0 else x0) in
  for curr_sr = (if dx > dy then x0 else y0) to (if dx > dy then x1 else y1) do
    err := !err -. lr;
    if !err < 0. then (curr_lr := !curr_lr + 1; err := !err +. sr);
    if dx > dy
      then single_aliased_pixel t curr_sr !curr_lr color
      else single_aliased_pixel t !curr_lr curr_sr color
done


let bresenham_n t x0 y0 x1 y1 color =
  let dx = float_of_int (Int.abs(x1 - x0)) in
  let dy = float_of_int (Int.abs(y1 - y0)) in
  let sr = if dx > dy then dx else dy in
  let lr = if dx > dy then dy else dx in
  let err = ref (sr /. 2.) in
  let curr_lr = ref (if dx > dy then y0 else x0) in
  for curr_sr = (if dx > dy then x0 else y0) downto (if dx > dy then x1 else y1) do
    err := !err -. lr;
    if !err < 0. then (curr_lr := !curr_lr + 1; err := !err +. sr);
    if dx > dy
      then single_aliased_pixel t curr_sr !curr_lr color
      else single_aliased_pixel t !curr_lr curr_sr color
  done

let bresenham t x0 y0 x1 y1 color =
  if x0 < x1 && y0 < y1 then bresenham t x0 y0 x1 y1 color else
  if x0 < x1 && y0 > y1 then bresenham_n t x1 y1 x0 y0 color else
  if x0 > x1 && y0 < y1 then bresenham_n t x0 y0 x1 y1 color else
  if x0 > x1 && y0 > y1 then bresenham t x1 y1 x0 y0 color



let () =
  Random.self_init ();
  let rgba32 = Rgba32.create 100 100 in
  try
    let red : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    single_aliased_pixel rgba32 1 9 red;
    bresenham rgba32 1 1 8 11 red;
    (* single_aliased_pixel rgba32 1 0 red;
    single_aliased_pixel rgba32 1 0 red; *)
    (* palette_preview rgba32 (build_palette 5);
    tarte_linzer rgba32 7 16 (build_palette 4); *)
    (* bresenham rgba32 6 3 1 4 red; *)
    (* bresenham rgba32 3 5 8 7 red; *)
    (* bresenham rgba32 8 9 4 7 red; *)
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e



(* ajouter les effets de bord sur bresenham *)