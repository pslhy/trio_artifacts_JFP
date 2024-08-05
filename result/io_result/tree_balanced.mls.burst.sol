fix (f : tree -> bool) =
  fun (x:tree) ->
    match x with
      | Leaf _ -> True
      | Node _ -> (match compare (height (Un_Node x . 1))
                           S (height (Un_Node x . 2)) with
                     | LT _ -> f (Un_Node x . 2)
                     | EQ _ -> f (Un_Node x . 1)
                     | GT _ -> False)