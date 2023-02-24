let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgba32 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgba32 t)
  with Failure m -> Format.printf "Error %s@." m

let _ver_strip t x y len rgb =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if x > dim_x || x < 0 || y > dim_y || y < 0 || y + len > dim_y then failwith "Out of image." else
  for y = y to y + len - 1 do
    (* for y = 0 to dim_y -1 do *)
      Rgba32.set t x y rgb
    (* done; *)
  done

let _hor_strip t x y len rgba =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if y > dim_y || y < 0 || x > dim_x || x < 0 || x + len > dim_x then failwith "Out of image." else
  (* if x > dim_x || x < 0 || x + len > dim_x then failwith "Out of image." else *)
  for x = x to x + len - 1 do
    (* for x = x to dim_x -1 do *)
    Rgba32.set t x y rgba
    (* done *)
  done

(* let _tarte_linzer t x len palette =
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
  with Failure _ -> () *)

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

(* let _palette_preview t palette =
  let _dim_x, dim_y = Images.size (Rgba32 t) in
  let len = Array.length palette in
  let strip_size = dim_y / len in
  let res = dim_y mod len in
  for i = 0 to len - 1 do
    hor_strip t (i*strip_size) strip_size palette.(i)
  done;
  hor_strip t (strip_size * len) res palette.(len-1) *)


let _draw_line t xs ys xe ye =
  (* let x = ref xs in *)
    let red : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
  for x = xs to xe do
    let ideal_y = ((ye - ys)/(xe - xs)) * (x - xs) + ys in
    Rgba32.set t x ideal_y red
  done

(* let line2 t x0 y0 x1 y1 color =
  let a = y1 - y0 in
  let b = x0 - x1 in
  let c = (x1 * y0) - (x0 * y1) in
  let rec fill x y =
    if y = y1 then ()
    else begin
      let rec search_next_node next_x = if a*next_x + b*(y+1) + c = 0 then next_x else search_next_node (next_x+1) in
      Format.printf "(%d;%d)@." x y;
      let next_x = search_next_node (x+1) in
      hor_strip t x y (next_x - x) color;
      fill (x + next_x) (y+1) end in
  fill x0 y0 *)

let bresenham t x0 y0 x1 y1 color =
  let dx = x1 - x0 in
  let dy = y1 - y0 in
  let err = ref (2*dy - dx) in
  let x = ref x0 in
  let y = ref y0 in
  Rgba32.set t !x !y color;
  while !x < x1 do
    x := !x + 1;
    if !err < 0 then err := !err + 2*dy else (y := !y + 1; err := !err + 2*(dy - dx));
    Rgba32.set t !x !y color;
  done

let () =
  Random.self_init ();
  let rgba32 = Rgba32.create 100 100 in
  try
    let red : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    (* alpha 0 -> 255 *)
    (* Rgba32.set rgba32 3 3 red; *)
    (* ver_strip rgba32 1 1 red; *)
    bresenham rgba32 4 4 7 7 red;
    (* hor_strip rgba32 8 7 3 red;
    ver_strip rgba32 8 7 3 red; *)
    (* Rgba32.set rgba32 15 15 red; *)
    (* tarte_linzer rgba32 8 5 (build_palette 7); *)
    (* palette_preview Rgba32 (build_palette 6); *)
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e
