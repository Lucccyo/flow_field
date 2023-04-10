type coordinate = {x: int; y: int}

type vector = {
  direction: float;
  norm: float;
  coordinate: coordinate;
}
(* direction is the angle with the x axis *)

val vector_from_angle_len: coordinate -> float -> float -> vector

val vector_from_coordinates: coordinate -> coordinate -> vector

val equal: vector -> vector -> bool

val nul: unit -> vector

(* val (+): vector -> vector -> vector *)

val dot_product: vector -> vector -> float
(* al-Kashi's theorem *)
