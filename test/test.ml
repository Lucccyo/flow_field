open Data.Vector

let () =
  Format.printf "miaou\n";
  let a = {x = 1; y = 1} in
  let b = {x = 2; y = 3} in
  let vec = vector_from_coordinates a b in
  Format.printf "direction = %f\tnorm = %f\t coordinate = (%d;%d)\n\n\n"
    vec.direction vec.norm vec.coordinate.x vec.coordinate.y;

  let a_ = {x = 1; y = 1} in
  let angle = 1.107149 in
  let length = 2.236068 in
  let vec2 = vector_from_angle_len a_ angle length in
  Format.printf "direction = %f\tnorm = %f\t coordinate = (%d;%d)\n\n\n"
  vec2.direction vec2.norm vec2.coordinate.x vec2.coordinate.y;
  ()


