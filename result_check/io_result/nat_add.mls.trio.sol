fix (f : nat * nat -> nat) =
  fun (x:nat * nat) ->
    match x . 0 with
      | O _ -> x . 1
      | S _ -> f (Un_S (x . 0), S (x . 1))