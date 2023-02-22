let () =
  let indexed = Index8.create_with 200 170 [] {max = 0; map =
    [|
      {r = 255; g = 51 ; b = 51 };
      {r = 255; g = 255; b = 0  };
      {r = 255; g = 128; b = 0  };
      {r = 0  ; g = 204; b = 204}
      |]} 40 (Bytes.of_string "Meow") in
  let filename = "meow.png" in
  let n, m = Images.size (Index8 indexed) in
  Format.printf "image size : %dx%d\n" n m;
  Images.save filename (Some Png) [] (Index8 indexed);
  (* So if img is of type Rgb24.t, then (Rgb24 img) has type Images.t. *)
  (* let n,m = Images.size img in *)
