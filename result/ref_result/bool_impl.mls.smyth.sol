fun (x:bool * bool) ->
  (fix (f : ) =
     fun (x1:) -> fun (x0:) -> match x1 with
                                 | True y1 -> x0
                                 | False y1 -> True)
    (x . 0) (x . 1)