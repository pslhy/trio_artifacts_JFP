fun (x:bool * bool) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) -> match x0 with
                      | True y1 -> x1
                      | False y1 -> False)
    (x . 0) (x . 1)