fun (x:bool * bool) ->
  (fix (f : ) =
     fun (x1:) -> fun (x0:) -> match x0 with
                                 | True y1 -> True
                                 | False y1 -> x1)
    (x . 0) (x . 1)