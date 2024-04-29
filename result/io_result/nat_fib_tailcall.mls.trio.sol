fix (f : nat * nat * nat -> nat) =
  fun (x:nat * nat * nat) ->
    match x . 0 with
      | O _ -> x . 1
      | S _ -> (match Un_S (x . 0) with
                  | O _ -> x . 0
                  | S _ -> add (f (Un_S (x . 0), Un_S (x . 0), Un_S (x . 0)))
                             (f
                                (Un_S (Un_S (x . 0)), Un_S (Un_S (x . 0)),
                                  Un_S (Un_S (x . 0)))))