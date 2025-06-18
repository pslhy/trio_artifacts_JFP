fix (f : tree -> bool) =
  fun (x:tree) ->
    match x with
      | Leaf _ -> True
      | Node _ -> (match Un_Node x . 2 with
                     | Leaf _ -> (match Un_Node x . 1 with
                                    | Leaf _ -> True
                                    | Node _ -> (match Un_Node (Un_Node x . 1)
                                                         . 1 with
                                                   | Leaf _ -> True
                                                   | Node _ -> False))
                     | Node _ -> (match Un_Node (Un_Node x . 2) . 1 with
                                    | Leaf _ -> (match Un_Node (Un_Node x . 2)
                                                         . 2 with
                                                   | Leaf _ -> f
                                                                 (Un_Node x .
                                                                    1)
                                                   | Node _ -> False)
                                    | Node _ -> False))