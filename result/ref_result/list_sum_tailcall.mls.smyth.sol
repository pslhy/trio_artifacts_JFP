fun (x:list * nat) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x1 with
           | Nil y1 -> x0
           | Cons y1 -> add (f (y1 . 1) x0) (y1 . 0))
    (x . 0) (x . 1)