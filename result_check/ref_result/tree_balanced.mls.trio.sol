fix (f : tree -> bool) =
  fun (x:tree) ->
    match x with
      | Leaf _ -> True
      | Node _ -> (match compare (height (Un_Node x . 1)) (height x) with
                     | EQ _ -> True
                     | GT _ -> False
                     | LT _ -> (match compare (height x)
                                        (height (Un_Node x . 2)) with
                                  | EQ _ -> True
                                  | GT _ -> True
                                  | LT _ -> False))