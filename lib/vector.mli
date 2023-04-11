type coordinate = { x: int; y: int }

type vector = {
  dir  : float;
  norm : float;
  c: coordinate
}

val null_vector: unit -> vector

val vector_from_angle_len: coordinate -> float -> float -> vector

val vector_from_coordinates: coordinate -> coordinate -> vector

val equal: vector -> vector -> bool

val dot_product: vector -> vector -> float
