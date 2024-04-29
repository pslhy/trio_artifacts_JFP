(** Helper module for deconstructing {!Lang.exp} values.

    See {!Desugar} for some inverse operations. *)

open Lang

val nat : exp -> int option
(** [nat] deconstructs the expression [S (... S (Z ()) ...)] into the integer
    [n], where the [S] constructor is nested [n] times
    (i.e., [S]{^[n]}[(Z ())]). *)

val listt : exp -> (exp list * typ list) option
(** [listt] deconstructs the expression

    {[Cons<taus> (e0, ... Cons<taus> (eN, Nil<taus>))]}

    into the pair [([e0, ..., eN], taus)]. *)
