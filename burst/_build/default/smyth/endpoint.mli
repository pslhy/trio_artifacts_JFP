(** Endpoints for clients using the {e Smyth} library.

    Two main endpoints are exposed: the {!section:solve} endpoint and the
    {!section:test} endpoint.
    A third endpoint, {!section:assertion}, is also provided for the more niche
    case of gathering information about assertions encoded in sketches.

    These endpoints take in the surface-level string representations of their
    inputs. *)

(** The type of errors that clients might receive. *)
type error =
  | TypeError of (Lang.exp * Type.error)
  | EvalError of string
  | TimedOut of float
  | NoSolutions
  | PartialNotSubsetFull
[@@deriving show]
    (** The given partial assertions are not a subset of the full assertions. *)

type 'a response =
  ('a, error) result
(** The response type that clients will receive. *)

(** {1:solve Solve} *)

type solve_result =
  { hole_fillings : (Lang.hole_name * Lang.exp) list list
    (** A list of hole fillings that satisfy the constraints of a sketch. *)
  ; time_taken : float
    (** The time taken to produce the valid hole fillings. *)
  }
(** The result of a successful "solve" operation. *)

val solve_program : Desugar.program -> solve_result response

val solve : sketch:string -> solve_result response
(** [solve sketch] tries to return a {!solve_result} that satisfies the
    assertions in [sketch]. *)

(** {1:test Test} *)

type test_result =
  { specification_assertion_count : int
    (** The number of assertions in the specification. *)
  ; assertion_count : int
    (** The number of assertions in the sketch. *)
  ; top_success : bool
    (** Whether or not the top-ranked solution is valid. *)
  ; top_recursive_success : bool
    (** Whether or not the top-ranked recursive solution is valid. *)
  ; time_taken : float
    (** The time taken to produce these results. *)
  }
(** The result of a successful "test" operation. *)

val test :
  specification:string ->
  sketch:string ->
  examples:string ->
  test_result response
(** [test specification sketch examples] synthesizes hole fillings for
    [sketch] using [examples], then tests these hole fillings for validity
    against [specification].

   In typical use, [specification] and [examples] are lists of assertions, and
   [sketch] is a partially-completed program with no assertions. *)

val test_assertions :
  specification:string ->
  sketch:string ->
  assertions:((Lang.exp * Lang.exp) list) ->
  test_result response
(** A convenience wrapper for {!test} that takes in a list of parsed assertions
    rather than a string. *)

(** {1:assertion Assertion Info} *)

val assertion_info :
  specification:string ->
  assertions:string ->
  (bool * Lang.exp list * Lang.exp) list response
(** [assertion_info specification assertions] returns a list consisting of
    values [(included, [x0, ..., xN], y)] for each assertion in
    [specification], which should each be of the form [f x0 ... xN == y].
    [included] is true if and only if the corresponding assertion is also in
    [assertions]. *)
