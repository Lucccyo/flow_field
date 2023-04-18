type vector = { dx: float; dy: float }

val null_vector: vector

val vector_from_angle_len: float -> float -> vector

val vector_from_coordinates: vector -> vector -> vector

val equal: vector -> vector -> bool

val dot_product: vector -> vector -> float

val pp_vector: Format.formatter -> vector -> unit
