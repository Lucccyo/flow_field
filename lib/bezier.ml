let ver_line t x y len rgb =
  let dim_x, dim_y = Images.size (Rgba32 t) in
  if x > dim_x || x < 0 || y > dim_y || y < 0 || y + len > dim_y then failwith "Out of image." else
  for y = y to y + len - 1 do
      Rgba32.set t x y rgb
  done

let hor_line t x y len rgba =
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

let tarte_linzer t ofs bs palette =
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

let rec build_palette size arr r g b a: Color.rgba array =
  let ofs = 50 / size in
  let open Random in
  if Array.length arr = size then arr else
  let red   = r + (let rand = ofs + (int 50) in if r + rand > 255 then 0 else rand) in
  let green = g + (let rand = ofs + (int 50) in if g + rand > 255 then 0 else rand) in
  let blue  = b + (let rand = ofs + (int 50) in if b + rand > 255 then 0 else rand) in
  build_palette size (Array.append arr [|{color = {r; g; b}; alpha = a}|]) red green blue a

let build_palette size a=
  let open Random in
  let ofs = 100 / size in
  build_palette size [||] (int ofs) (int ofs) (int ofs) a

let palette_preview t palette =
  let _dim_x, dim_y = Images.size (Rgba32 t) in
  let len = Array.length palette in
  let strip_size = dim_y / len in
  let res = dim_y mod len in
  for i = 0 to len - 1 do
    hor_strip t (i*strip_size) strip_size palette.(i)
  done;
  hor_strip t (strip_size * len) res palette.(len-1)


let draw_line t xs ys xe ye =
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

let curve_bezier t x_list y_list color =
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

let curve_points _t x_list y_list =
  let o = List.length x_list in
  if o <> List.length y_list then failwith "missing corresponding coordonate";
  let fx_list = List.map (fun x -> float_of_int x) x_list in
  let fy_list = List.map (fun y -> float_of_int y) y_list in
  let n = o - 1 in
  let step = 0.01 in
  let rec iter ti _ox _oy res =
    let x,y = if ti >= 1.0 then
      List.hd (List.rev x_list),
      List.hd (List.rev y_list)
    else
      int_of_float ( bezier n 0 ti fx_list ),
      int_of_float ( bezier n 0 ti fy_list ) in
      (* Rgba32.set t x y {color = {r = 0; g = 0; b = 0}; alpha = 255}; *)
    if ti < 1.0 then iter (ti +. step) x y (List.append res [(x, y)]) else res in
  iter step (List.hd x_list) (List.hd y_list) []

let rec ribbon t starts controls ends color =
  if starts = [] then () else begin
  curve_bezier t
    [fst (List.hd starts);
     fst (List.hd controls);
     fst (List.hd ends)]
    [snd (List.hd starts);
     snd (List.hd controls);
     snd (List.hd ends)] color;
  ribbon t (List.tl starts) (List.tl controls) (List.tl ends) color end