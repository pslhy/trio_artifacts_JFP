fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Nil y1 -> x0
      | Cons y1 -> append (f (y1 . 1)) Cons (y1 . 0, Nil)