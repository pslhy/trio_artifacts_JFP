fix (f : (nat -> nat -> nat) * nat * list -> nat) =
  fun (x:(nat -> nat -> nat) * nat * list) ->
    match x . 2 with
      | Nil _ -> x . 1
      | Cons _ -> sum
                    (f (count_odd, Un_Cons (x . 2) . 0, Un_Cons (x . 2) . 1))
                    (x . 1)