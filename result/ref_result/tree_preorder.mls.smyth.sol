fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Node y1 -> Cons (y1 . 1, append (f (y1 . 0)) (f (y1 . 2)))
      | Leaf y1 -> Nil