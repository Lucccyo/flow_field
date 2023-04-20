open Data.Bezier
open Data.Vector

let build t =
  let filename = "img.png" in
  try
    Png.save filename [Images.Save_Quality 1] (Rgba32 t);
    if Array.length Sys.argv - 1 = 1 then
      let store = "musée/" ^ Sys.argv.(1) ^ ".png" in
      Png.save store [Images.Save_Quality 1] (Rgba32 t)
  with Failure m -> Format.printf "Error %s@." m

(* let ii2v idx idy = { dx = float_of_int idx ; dy = float_of_int idy } *)

let dot_product v1 v2 ss =
  (* if (v1.dx *. v1.dx +. v1.dy *. v1.dy > ss *. ss) then Format.printf "! %f@." (v1.dx *. v1.dx +. v1.dy *. v1.dy); *)
  (* if (v2.dx *. v2.dx +. v2.dy *. v2.dy > 2. *. ss *. ss) then Format.printf "? %f@." (v2.dx *. v2.dx +. v2.dy *. v2.dy); *)
  (*let p = (v1.dx /. ss) *. (v2.dx /. ss) +. (v1.dy /. ss) *. (v2.dy /. ss) in*)
  let p = dot_product v1 v2 /. ss /. ss in
  (* Format.printf "%f@." p; *)
  p

let dot_products x y grid size_square =
  let ss = float_of_int size_square in
  let i = Float.to_int (y /. ss) in
  let j = Float.to_int (x /. ss) in
  let dx = x -. float_of_int (j * size_square) in
  let dy = y -. float_of_int (i * size_square) in
  dot_product grid.(i).(j) {dx; dy} ss,
  dot_product grid.(i).(j + 1) {dx = dx -. ss; dy} ss,
  dot_product grid.(i + 1).(j + 1) {dx = dx -. ss; dy = dy -. ss} ss,
  dot_product grid.(i + 1).(j) {dx; dy = dy -. ss} ss,
  dx /. ss,
  dy /. ss

(*
let rand_lst size shift max =
  let open Random in
  List.init size (fun _ -> shift + int max)

let map value min1 max1 min2 max2 = min2 +. ( max2 -. min2 ) *. (( value -. min1 ) /. ( max1 -. min1 ))

let dot_p node x y =
  let node_distance = vector_from_coordinates {x = node.x; y = node.y} {x; y} in
  pp_vector node.gradiant_vector;
  pp_vector node_distance;
  dot_product node.gradiant_vector node_distance
*)

let vmin = ref 0.
let vmax = ref 0.

let calc_angle_orig x y grid size_square =
  let psw, pse, pne, pnw, xf, yf = dot_products x y grid size_square in
  let fade t = 6.*.(t**5.) -. 15.*.(t**4.) +. 10.*.(t**3.) in
  let lerp t v1 v2 = v1 +. t *. (v2 -. v1) in
  let u = fade xf in
  let v = fade yf in
  let r = lerp u (lerp v psw pnw) (lerp v pse pne) in
  (* if r < !vmin then (vmin := r; Format.printf "%f %f@." !vmin !vmax); *)
  (* if r > !vmax then (vmax := r; Format.printf "%f %f@." !vmin !vmax); *)
  3. *. Float.pi *. r

let calc_angle_wiki x y grid size_square =
  let psw, pse, pne, pnw, sx, sy = dot_products x y grid size_square in
  let ipl a0 a1 w = (a1 -. a0) *. ((w *. (w *. 6.0 -. 15.0) +. 10.0) *. w *. w *. w) +. a0 in
  let ix0 = ipl psw pse sx in
  let ix1 = ipl pnw pne sx in
  let r = ipl ix0 ix1 sy in
  (* if r < !vmin then (vmin := r; Format.printf "%f %f@." !vmin !vmax); *)
  (* if r > !vmax then (vmax := r; Format.printf "%f %f@." !vmin !vmax); *)
  3. *. Float.pi *. r

let rec iter t n cur_x cur_y color grid size_square =
  let pas = 3. in
  if n = 0 then () else (
    let angle = calc_angle_orig (float_of_int cur_x) (float_of_int cur_y) grid size_square in
(*Format.printf "%f@." angle;*)
    let next_x = cur_x + int_of_float(pas *. Float.cos(angle)) in
    let next_y = cur_y + int_of_float(pas *. Float.sin(angle)) in
    bresenham t cur_x cur_y next_x next_y color;
    iter t (n-1) next_x next_y color grid size_square )

let draw_line t start_x start_y size grid size_square =
  let red : Color.rgba = {
      color = { r = Random.int 255; g = Random.int 255; b = 200 };
      alpha = Random.int 255 } in
  try iter t size start_x start_y red grid size_square
  with Images.Out_of_image -> ()

(*
let print_grid g =
  for y = 0 to Array.length g - 1 do
    for x = 0 to Array.length g.(0) - 1 do
      pp_node g.(x).(y)
    done;
    Format.printf "\n"
  done
*)

let grid dim_image size_square =
  Array.init ((dim_image / size_square) + 1) (fun y_grid ->
  Array.init ((dim_image / size_square) + 1) (fun x_grid ->
      let angle = Random.float (2. *. Float.pi) in
      vector_from_angle_len angle (float_of_int size_square)))

let () =
  Random.self_init ();
  let image_size = 2000 in
  let rgba32 = Rgba32.create image_size image_size in
  try
    let black : Color.rgba = {color = {r = 0; g = 0; b = 0}; alpha = 255} in
    let red   : Color.rgba = {color = {r = 255; g = 0; b = 0}; alpha = 255} in
    hor_strip rgba32 0 image_size black;
    let size_square = 50 in
    let g = grid image_size size_square in
    (* catch 1999 1999 g size_square; *)
    (* Format.printf "\n>> %f\n" (calc_angle 33 33 g 250); *)
    (*
    for _ = 0 to 300 do
      draw_line rgba32 (Random.int 2000) (Random.int 2000) (10 + Random.int 300) g size_square;
    done;
    *)

    let h_of_angle h ceil =
      let max = 2. *. Float.pi in
      let min = -. max in
      let h = (h -. min) *. ceil /. (max -. min) in
      let h = if h < 0. then 0. else
        (* h in *)
        if h > ceil then ceil else h in
      int_of_float h (*mod int_of_float ceil*)
    in

    for x = 0 to 1999 do for y = 0 to 1999 do
      (* if x mod size_square = 0 || y mod size_square = 0 then *)
      let s = ref 0. in
      let fx = float_of_int x and fy = float_of_int y in
      let rec fbm n l h =
        if n = 0 then h else
        let a = Float.ldexp 1. (1 - n) in
        let h = h +. a *. calc_angle_wiki (fx /. l) (fy /. l) g size_square in
        fbm (n - 1) (l *. 2.) h
      in
      let h = fbm 5 1. 0. in
(*
      if h < !vmin then (vmin := h; Format.printf "%f %f@." !vmin !vmax);
      if h > !vmax then (vmax := h; Format.printf "%f %f@." !vmin !vmax);
*)
      (*
      let h = h_of_angle h 255. in
      let c = {Color.color = {r = 0; g = h; b = 0}; alpha = 255} in
      *)
      let h = h_of_angle h 255. in
      let c = if h < 128 then
        {Color.color = {r = 0; g = 0; b = 30 + h * 4 / 3}; alpha = 255} else
        {Color.color = {r = 0; g = h; b = 0}; alpha = 255} in
      (*
      let h = h_of_angle h 360. in
      let t = (h mod 60) * 255 / 60 in
      let q = 255 - t in
      let c = match h / 60 with
      | 0 -> {Color.color = {r = 255; g = t; b = 0}; alpha = 255}
      | 1 -> {Color.color = {r = q; g = 255; b = 0}; alpha = 255}
      | 2 -> {Color.color = {r = 0; g = 255; b = t}; alpha = 255}
      | 3 -> {Color.color = {r = 0; g = q; b = 255}; alpha = 255}
      | 4 -> {Color.color = {r = t; g = 0; b = 255}; alpha = 255}
      | 5 -> {Color.color = {r = 255; g = 0; b = q}; alpha = 255}
      | _ -> {Color.color = {r = 0;   g = 0; b = 0}; alpha = 255}
      in
      *)
      Rgba32.set rgba32 x y c
    done done;
    build rgba32
  with Failure e -> Format.printf "ERROR: %s@." e