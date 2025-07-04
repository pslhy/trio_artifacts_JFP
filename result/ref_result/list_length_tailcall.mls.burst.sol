fix (f : list * nat -> nat) =
  fun (x:list * nat) ->
    match x . 0 with
      | Nil _ -> x . 1
      | Cons _ -> S (f (Un_Cons (x . 0) . 1, x . 1))