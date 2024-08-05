fix (f : tree -> bool) =
  fun (x:tree) ->
    match x with
      | Leaf _ -> True
      | Node _ -> (match compare (height (Un_Node x . 2)) 1 with
                     | EQ _ -> f (Un_Node x . 1)
                     | GT _ -> False
                     | LT _ -> (match compare 1 (height (Un_Node x . 1)) with
                                  | EQ _ -> True
                                  | GT _ -> True
                                  | LT _ -> False))