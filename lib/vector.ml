type coordinate = { x: int; y: int }

type vector = {
  dir : float;
  norm: float;
  c: coordinate
}

let (fli) = float_of_int

let (ifl) = int_of_float

let null_vector () = { dir = 0.0; norm = 0.0; c = { x = 0; y = 0 } }

let vector_from_angle_len start_point angle length =
  { dir  = angle;
    norm = length;
    c = { x = ifl (Float.cos angle *. length) + start_point.x;
          y = ifl (Float.sin angle *. length) + start_point.y } }

let vector_from_coordinates start_point end_point =
  { dir  = Float.atan ( (fli end_point.y -. fli start_point.y) /.
                        (fli end_point.x -. fli start_point.x) );
    norm = Float.( sqrt (pow (fli (end_point.x - start_point.x)) 2. +.
                         pow (fli (end_point.y - start_point.y)) 2.) );
    c = { x = end_point.x - start_point.x; y = end_point.y - start_point.y } }

let equal v1 v2 = v1.dir = v2.dir && v1.norm = v2.norm

let dot_product v1 v2 = fli ((v1.c.x * v2.c.x) + (v1.c.y * v2.c.y))
