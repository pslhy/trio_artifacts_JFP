fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Nil y1 -> Nil
      | Cons y1 -> snoc (y1 . 1) (y1 . 0)