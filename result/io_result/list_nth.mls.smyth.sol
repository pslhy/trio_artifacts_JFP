fun (x:list * nat) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x1 with
           | Nil y1 -> 0
           | Cons y1 -> (match x0 with
                           | S y2 -> f (y1 . 1) y2
                           | O y2 -> y1 . 0))
    (x . 0) (x . 1)