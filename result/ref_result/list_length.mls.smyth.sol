fix (f : ) =
  fun (x0:) -> match x0 with
                 | Nil y1 -> 0
                 | Cons y1 -> S (f (y1 . 1))