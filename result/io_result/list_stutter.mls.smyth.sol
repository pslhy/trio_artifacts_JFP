fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Nil y1 -> Nil
      | Cons y1 -> Cons (y1 . 0, Cons (y1 . 0, f (y1 . 1)))