fix (f : ) =
  fun (x0:) ->
    match x0 with
      | Nil y1 -> None
      | Cons y1 -> (match y1 . 1 with
                      | Nil y2 -> Some (y1 . 0)
                      | Cons y2 -> f (y1 . 1))