fun (x:list * list) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x1 with
           | Nil y1 -> x0
           | Cons y1 -> Cons (y1 . 0, f (y1 . 1) x0))
    (x . 0) (x . 1)