let cumsum (arr : float array) =
  Array.mapi
    (fun n _ -> n + 1 |> Array.sub arr 0 |> Array.fold_left ( +. ) 0.)
    arr
