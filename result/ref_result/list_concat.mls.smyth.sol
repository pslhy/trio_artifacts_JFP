fix (f : ) =
  fun (x0:) ->
    match x0 with
      | LNil y1 -> Nil
      | LCons y1 -> append (y1 . 0) (f (y1 . 1))