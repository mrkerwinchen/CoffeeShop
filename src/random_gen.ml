let runif_std () =
  let _ = Random.self_init () in
  Random.float 1.

let rec runif ~a ~b =
  let _ = Random.self_init () in
  let value = Random.float b in
  if value >= a then value else runif ~a ~b

let rec runif_disc ~a ~b =
  let _ = Random.self_init () in
  let value = Random.int (b + 1) in
  if value >= a then value else runif_disc ~a ~b

let rnorm_std () =
  cos (2. *. Float.pi *. runif_std ()) *. sqrt (-2. *. log (runif_std ()))

let rnorm ~mu ~sigma = mu +. (sigma *. rnorm_std ())