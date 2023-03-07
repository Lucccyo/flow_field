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


let merge src dest = (* I am not sure... *)
  let open Color in
  let check v = if v < 0 then 0 else if v > 255 then 255 else v in
  if dest.alpha = 0 then src else
  if dest.alpha = 255 then dest else
  let alpha' = 255 - dest.alpha in
  let c = {
    r = check ((alpha' * src.color.r * src.alpha / 255 +
                dest.color.r * dest.alpha) / 255);
    g = check ((alpha' * src.color.g * src.alpha / 255 +
                dest.color.g * dest.alpha) / 255);
    b = check ((alpha' * src.color.b * src.alpha / 255 +
                dest.color.b * dest.alpha) / 255);
  } in
  { color = c; alpha = check (255 - alpha' * (255 - src.alpha) / 255); }

let add t x y nc  =
  let oc = Rgba32.get t x y in
  Rgba32.set t x y (merge oc nc)

let single_aliased_pixel t x y color =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  let float_alpha = float_of_int color.Color.alpha in
  let lowpacity      : Color.rgba = {color with alpha = int_of_float (0.1 *. float_alpha)} in
  let middle_opacity : Color.rgba = {color with alpha = int_of_float (0.3 *. float_alpha)} in
  let top_opacity    : Color.rgba = {color with alpha = int_of_float (1. *. float_alpha)} in
  if x > 0  && y > 0 then add t (x - 1) (y - 1) lowpacity else ();
  if y > 0 then add t x (y - 1) middle_opacity else ();
  if x < dim_x && y > 0 then add t (x + 1) (y - 1) lowpacity else ();
  if x > 0 then add t (x - 1) y middle_opacity else ();
  add t x y top_opacity;
  if x < dim_x then add t (x + 1) y middle_opacity else ();
  if x > 0 && y < dim_y then add t (x - 1) (y + 1) lowpacity else ();
  if y < dim_x then add t x (y + 1) middle_opacity else ();
  if x < dim_x && y < dim_y then add t (x + 1) (y + 1) lowpacity else ();
  ()

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
  if dx > dy then
    for curr_sr = x0 to x1 do
      err := !err -. lr;
      if !err < 0. then (curr_lr := !curr_lr - 1; err := !err +. sr);
      if dx > dy
        then single_aliased_pixel t curr_sr !curr_lr color
        else single_aliased_pixel t !curr_lr curr_sr color
    done
  else
    for curr_sr = y0 downto y1 do
      err := !err -. lr;
      if !err < 0. then (curr_lr := !curr_lr + 1; err := !err +. sr);
      if dx > dy
        then single_aliased_pixel t curr_sr !curr_lr color
        else single_aliased_pixel t !curr_lr curr_sr color
    done


let bresenham t x0 y0 x1 y1 color =
  if x0 <= x1 && y0 <= y1 then bresenham   t x0 y0 x1 y1 color else
  if x0 <= x1 && y0 >  y1 then bresenham_n t x0 y0 x1 y1 color else (* <- *)
  if x0 >  x1 && y0 <= y1 then bresenham_n t x1 y1 x0 y0 color else
  if x0 >  x1 && y0 >  y1 then bresenham   t x1 y1 x0 y0 color

let rec fact n = if n = 0 || n = 1 then 1 else n * fact (n - 1)

let bin_coeff k n = fact n / (fact k * fact (n - k))

let rec bezier n i t p_list =
  match p_list with
  | [] -> if i > 0 then 0. else assert false
  | pi :: tl ->
    let fbin_coeff = float_of_int (bin_coeff i n) in
    let fi = float_of_int i in
    let fn = float_of_int n in
    bezier n (i + 1) t tl
    +. ((fbin_coeff) *. ((1. -. t)**(fn -. fi)) *. (t**fi) *. pi)

let bezier t x_list y_list color =
  let o = List.length x_list in
  if o <> List.length y_list then failwith "missing corresponding coordonate";
  let fx_list = List.map (fun x -> float_of_int x) x_list in
  let fy_list = List.map (fun y -> float_of_int y) y_list in
  let n = o - 1 in
  let step = 0.01 in
  let rec iter ti ox oy =
    let x,y = if ti >= 1.0 then
      List.hd (List.rev x_list),
      List.hd (List.rev y_list)
    else
      int_of_float ( bezier n 0 ti fx_list ),
      int_of_float ( bezier n 0 ti fy_list ) in
    (* Format.printf "%d %d %d %d @." ox oy x y; *)
    (*if Rgba32.get t x y <> color then*)
    bresenham t ox oy x y color;
    if ti < 1.0 then iter (ti +. step) x y in
  iter step (List.hd x_list) (List.hd y_list)

let rand_lst size shift max =
  let open Random in
  List.init size (fun _ -> shift + int max)

let () =
  Random.self_init ();
  let rgba32 = Rgba32.create 300 300 in
  try
    let open Color in
    let palette = [|{color = {r = 255; g = 0; b = 0}; alpha = 255} ;
                    {color = {r = 0;   g = 255; b = 0}; alpha = 255};
                    {color = {r = 0;   g = 0; b = 255}; alpha = 255};
                    {color = {r = 255;   g = 0; b = 255}; alpha = 255};
                    {color = {r = 0;   g = 0; b = 0}; alpha = 255}|] in
    let white : Color.rgba = {color = {r = 255; g = 255; b = 255}; alpha = 255} in
    hor_strip rgba32 0 300 white;
    (* bezier rgba32 [20; 70; 100; 120; 200; 120] [120; 20; 100; 30; 70; 100] red;
    Rgba32.set rgba32 20  120 green;
    Rgba32.set rgba32 70  20  green;
    Rgba32.set rgba32 100 100 green;
    Rgba32.set rgba32 120 30  green;
    Rgba32.set rgba32 200 70  green;
    Rgba32.set rgba32 120 100 green;


    bezier rgba32 [70; 200; 20; 150] [250; 150; 150; 250] red;
    Rgba32.set rgba32 70  250 blue;
    Rgba32.set rgba32 200 150 green;
    Rgba32.set rgba32 20  150 blue;
    Rgba32.set rgba32 150 250 blue; *)

    (* let palette = build_palette 5 in *)
    let r  = 42 in
    for _ = 0 to r do
      (* let rp = Random.int (Array.length palette) in *)
      let rp = {color = {r = Random.int 255; g = Random.int 255; b = Random.int 255}; alpha = 255} in
      let size = 5 in
      let xl = (rand_lst size 4 290) in
      let yl = (rand_lst size 4 290) in
      bezier rgba32 xl yl rp;
      (*for i = 0 to size - 1 do
        Rgba32.set rgba32 (List.nth xl i) (List.nth yl i) palette.(i)
      done;*)
      ()
    done;

    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e