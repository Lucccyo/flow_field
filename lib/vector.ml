type coordinate = {x: int; y: int}

type vector = {
  direction: float;
  norm: float;
  coordinate: coordinate;
}

let (f) = float_of_int

let vector_from_angle_len start_point angle length =
  { direction = angle;
    norm = length;
    coordinate =
      { x = int_of_float (Float.cos(angle) *. length) + start_point.x;
        y = int_of_float (Float.sin(angle) *. length) + start_point.y;}
  }

let vector_from_coordinates start_point end_point =
  { direction = Float.atan( (f end_point.y -. f start_point.y) /. (f end_point.x -. f start_point.x) );
    norm =  Float.sqrt(Float.pow (f (end_point.x - start_point.x)) 2. +. Float.pow (f (end_point.y - start_point.y)) 2.);
    coordinate = { x = end_point.x - start_point.x; y = end_point.y - start_point.y };
  }


let equal v1 v2 = v1.direction = v2.direction && v1.norm = v2.norm

let (=) = equal

let sum v1 v2 = vector_from_coordinates v1.coordinate v2.coordinate

let (+) = sum

(* let dot_product v1 v2 = *)

(* let dot_product v1 v2 = *)
  (* let norm2_v1 = Float.pow v1.norm 2. in
  let norm2_v2 = Float.pow v2.norm 2. in
  let v3 = v1 + v2 in
  let norm2_v3 = Float.pow v3.norm 2. in
  1. /. 2. *. (norm2_v3 -. norm2_v1 -. norm2_v2) *)