fix (f : list * nat -> nat) =
  fun (x:list * nat) ->
    match x . 0 with
      | Nil _ -> x . 1
      | Cons _ -> f (Un_Cons (x . 0) . 1, S (x . 1))