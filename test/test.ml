open Data.Vector

let () =
  Format.printf "miaou\n";
  let a = {x = 0; y = 0} in
  let b = {x = 2; y = 3} in
  let vec = vector_from_coordinates a b in
  Format.printf "direction = %f\tnorm = %f\t coordinate = (%d;%d)\n\n\n"
    vec.direction vec.norm vec.coordinate.x vec.coordinate.y;

  let a_ = {x = 0; y = 0} in
  let b_ = {x = 4; y = 5} in
  let vec2 = vector_from_coordinates a_ b_ in
  Format.printf "direction = %f\tnorm = %f\t coordinate = (%d;%d)\n\n\n"
  vec2.direction vec2.norm vec2.coordinate.x vec2.coordinate.y;

  Format.printf "produit scalaire = %f\n\n\n" (dot_product vec vec2);
  ()


