fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Nil y1 -> x0
      | Cons y1 -> snoc (f (y1 . 1)) (y1 . 0)