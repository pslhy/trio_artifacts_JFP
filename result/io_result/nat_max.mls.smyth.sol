fun (x:nat * nat) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x0 with
           | S y1 -> (match x1 with
                        | S y2 -> S (f y2 y1)
                        | O y2 -> x0)
           | O y1 -> (match x1 with
                        | S y2 -> x1
                        | O y2 -> 0))
    (x . 0) (x . 1)