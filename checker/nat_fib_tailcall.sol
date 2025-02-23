fix (f : nat * nat * nat -> nat) =
    fun (x:nat * nat * nat) ->
        match x . 0 with
        | O -> x . 1
        | S _ -> f [Un_S (x . 0), x . 2, add (x . 1) (x . 2)]