fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Node y1 -> S (sum (f (y1 . 0)) (f (y1 . 2)))
      | Leaf y1 -> 0