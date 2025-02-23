(** Hole context cleanup. *)

open Lang

val clean :
  hole_ctx -> hole_filling -> (hole_name * exp) list option
(** [clean delta hf] first propagates the expressions in the hole-filling [hf]
    upward to generate a new hole-filling with no expression holes (except for
    self-recursive ones, e.g. from the {e Defer} rule defined in {b Figure 8} of
    the ICFP 2020 paper).
    It then removes any unused holes from the resulting hole-filling and returns
    a list of hole-to-expression bindings. *)
