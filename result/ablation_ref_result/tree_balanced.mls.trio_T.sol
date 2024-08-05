fix (f : tree -> bool) =
  fun (x:tree) ->
    match x with
      | Leaf _ -> True
      | Node _ -> (match compare (height (Un_Node x . 1)) 1 with
                     | EQ _ -> True
                     | GT _ -> (match Un_Node x . 2 with
                                  | Leaf _ -> False
                                  | Node _ -> True)
                     | LT _ -> (match compare 1 (height (Un_Node x . 2)) with
                                  | EQ _ -> True
                                  | GT _ -> True
                                  | LT _ -> False))