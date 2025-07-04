fix (f : list * list -> list) =
  fun (x:list * list) ->
    match x . 0 with
      | Nil _ -> x . 0
      | Cons _ -> (match Un_Cons (x . 0) . 0 with
                     | O _ -> Cons (Un_Cons (x . 0) . 0, x . 0)
                     | S _ -> Cons (Un_Cons (x . 0) . 0, Cons (0, x . 1)))