fix (f : tree -> bool) =
    fun (x : tree) ->
        match x with
        | Leaf _ -> True
        | Node _ -> (match compare (height Un_Node(x).1) (height Un_Node(x).2) with
                          | LT _ -> (match compare S((height Un_Node(x).1)) (height Un_Node(x).2) with
                                    | LT _ -> False
                                    | EQ _ -> band (f Un_Node(x).2) (f Un_Node(x).1)
                                    | GT _ -> band (f Un_Node(x).2) (f Un_Node(x).1) )
                          | EQ _ -> band (f Un_Node(x).2) (f Un_Node(x).1) 
                          | GT _ -> (match compare S((height Un_Node(x).2)) (height Un_Node(x).1) with
                                    | LT _ -> False
                                    | EQ _ -> band (f Un_Node(x).2) (f Un_Node(x).1)
                                    | GT _ -> band (f Un_Node(x).2) (f Un_Node(x).1) ))