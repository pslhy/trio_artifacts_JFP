fun (x:list * nat) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x1 with
           | Nil y1 -> Cons (x0, Nil)
           | Cons y1 -> (match compare x0 (y1 . 0) with
                           | LT y2 -> Cons (x0, x1)
                           | GT y2 -> Cons (y1 . 0, f (y1 . 1) x0)
                           | EQ y2 -> Cons (x0, y1 . 1)))
    (x . 0) (x . 1)