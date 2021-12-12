let cumsum (arr : float array) =
  Array.mapi
    (fun n _ -> n + 1 |> Array.sub arr 0 |> Array.fold_left ( +. ) 0.)
    arr

let rec make_rep_array i j l rep : float array =
  if i > j then l |> Array.of_list else make_rep_array i (j - 1) (rep :: l) rep

let rec make_seq_array i j l =
  if i > j then l |> Array.of_list else make_seq_array i (j -. 1.) (j :: l)

let rec make_0_array i j l = make_rep_array i j l 0.

let arr_seek_val arr f = Array.fold_left f arr.(0) arr

let arr_max_val arr = arr_seek_val arr max

let arr_min_val arr = arr_seek_val arr min

let rescale_map_transform (arr : float array) (rescale_min : float)
    (rescale_max : float) : float array =
  let arr_min = arr_min_val arr in
  let arr_max = arr_max_val arr in
  let transform_function value =
    (rescale_max -. rescale_min) /. (arr_max -. arr_min) *. (value -. arr_min)
  in
  Array.map transform_function arr

let stretch_array (arr1 : float array) (arr2 : float array) :
    float array * float array =
  let arr1_len = Array.length arr1 in
  let arr2_len = Array.length arr2 in
  if arr1_len = arr2_len then (arr1, arr2)
  else if arr1_len > arr2_len then
    let diff = arr1_len - arr2_len in
    let new_arr = Array.append arr2 (make_0_array 1 diff []) in
    (arr1, new_arr)
  else
    let diff = arr2_len - arr1_len in
    let new_arr = Array.append arr1 (make_0_array 1 diff []) in
    (new_arr, arr2)

let fahrenheit_to_celsius fahrenheit = (fahrenheit -. 32.) *. 5. /. 9.

let round_n n f =
  let n = Int.to_float n in
  let prod = 10. *. n in
  (f *. prod |> Float.round) /. prod

let round_2 = round_n 2

let sum_arr arr = Array.fold_left ( +. ) 0. arr
