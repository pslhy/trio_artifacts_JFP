fun (x:nat * list) ->
  (fix (f : ) =
     fun (x1:) ->
       fun (x0:) ->
         match x0 with
           | Nil y1 -> Nil
           | Cons y1 -> (match x1 with
                           | S y2 -> Cons (y1 . 0, f y2 (y1 . 1))
                           | O y2 -> Nil))
    (x . 0) (x . 1)