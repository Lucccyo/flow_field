type vector = { dx: float; dy: float }

let null_vector = { dx = 0.; dy = 0. }

let vector_from_angle_len angle length =
  { dx = Float.cos angle *. length;
    dy = Float.sin angle *. length }

let vector_from_coordinates start_point end_point =
  { dx = end_point.dx -. start_point.dx;
    dy = end_point.dy -. start_point.dy }

let equal v1 v2 = v1 = v2

let dot_product v1 v2 = (v1.dx *. v2.dx) +. (v1.dy *. v2.dy)

let pp_vector fmt v =
  Format.fprintf fmt "{dx = %f ; dy = %f}" v.dx v.dy