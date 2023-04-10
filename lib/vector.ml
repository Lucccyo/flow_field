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

(* let (=) = equal *)

let nul () = { direction = 0.; norm = 0.; coordinate = { x = 0; y = 0 }; }

let dot_product v1 v2 = f ((v1.coordinate.x * v2.coordinate.x) + (v1.coordinate.y * v2.coordinate.y))
