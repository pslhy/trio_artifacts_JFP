(** the file is mostly borrowed from Smyth *)
open Core
open Lang

(* Helpers *)

let rec list_compress : 'a list -> 'a list =
 fun xs ->
  match xs with
  | a :: b :: rest ->
      if a = b then
        list_compress (b :: rest)
      else
        a :: list_compress (b :: rest)
  | _ -> xs

let rec list_even_parity : bool list -> bool =
 fun bs ->
  match bs with
  | [] -> true
  | head :: tail ->
      if head then
        not (list_even_parity tail)
      else
        list_even_parity tail

let rec list_pairwise_swap : 'a list -> 'a list option =
 fun xs ->
  match xs with
  | [] -> Some []
  | [ _ ] -> None
  | x1 :: x2 :: tail ->
      Option.map
        ~f:(fun flipped -> x2 :: x1 :: flipped)
        (list_pairwise_swap tail)

let rec list_sorted_insert : 'a -> 'a list -> 'a list =
 fun y xs ->
  match xs with
  | [] -> [ y ]
  | head :: tail ->
      if y < head then
        y :: xs
      else if y = head then
        xs
      else
        head :: list_sorted_insert y tail

(* References *)

type ('i, 'o) reference = {
  function_name : string;
  k_max : int;
  d_in : 'i Denotation.t;
  d_out : 'o Denotation.t;
  (* extensive test cases to check the correctness of synthesized programs *)
  assertion : ('i * 'o) list; 
  (* random input generator *)
  input : 'i Sample2.gen; 
  (* reference implementation for generating corresponding outputs for randomly generated inputs *)
  func : 'i -> 'o; 
  background : string list;
}

type 'a reference_projection = { proj : 'i 'o. ('i, 'o) reference -> 'a }

let all : 'a reference_projection -> (string * 'a) list =
 fun { proj } ->
  [
    ( "bool_band",
      proj
        {
          function_name = "bool_band";
          k_max = 4;
          d_in = Denotation.args2 Denotation.bool Denotation.bool;
          d_out = Denotation.bool;
          assertion =
            [
              ((true, true), true);
              ((true, false), false);
              ((false, true), false);
              ((false, false), false);
            ];
          input = Sample2.pair Sample2.bool Sample2.bool;
          func =
            (let f : bool * bool -> bool = fun (x, y) -> x && y in
             f);
          background = [];
        } );
    ( "bool_bor",
      proj
        {
          function_name = "bool_bor";
          k_max = 4;
          d_in = Denotation.args2 Denotation.bool Denotation.bool;
          d_out = Denotation.bool;
          assertion =
            [
              ((true, true), true);
              ((true, false), true);
              ((false, true), true);
              ((false, false), false);
            ];
          input = Sample2.pair Sample2.bool Sample2.bool;
          func =
            (let f : bool * bool -> bool = fun (x, y) -> x || y in
             f);
          background = [];
        } );
    ( "bool_impl",
      proj
        {
          function_name = "bool_impl";
          k_max = 4;
          d_in = Denotation.args2 Denotation.bool Denotation.bool;
          d_out = Denotation.bool;
          assertion =
            [
              ((true, true), true);
              ((true, false), false);
              ((false, true), true);
              ((false, false), true);
            ];
          input = Sample2.pair Sample2.bool Sample2.bool;
          func =
            (let f : bool * bool -> bool = fun (x, y) -> (not x) || y in
             f);
          background = [];
        } );
    ( "bool_neg",
      proj
        {
          function_name = "bool_neg";
          k_max = 2;
          d_in = Denotation.bool;
          d_out = Denotation.bool;
          assertion = [ (true, false); (false, true) ];
          input = Sample2.bool;
          func =
            (let f : bool -> bool = fun x -> not x in
             f);
          background = [];
        } );
    ( "bool_xor",
      proj
        {
          function_name = "bool_xor";
          k_max = 4;
          d_in = Denotation.args2 Denotation.bool Denotation.bool;
          d_out = Denotation.bool;
          assertion =
            [
              ((true, true), false);
              ((true, false), true);
              ((false, true), true);
              ((false, false), false);
            ];
          input = Sample2.pair Sample2.bool Sample2.bool;
          func =
            (let f : bool * bool -> bool = fun (x, y) -> not (Bool.equal x y) in
             f);
          background = [];
        } );
    ( "nat_iseven",
      proj
        {
          function_name = "nat_iseven";
          k_max = 4;
          d_in = Denotation.int;
          d_out = Denotation.bool;
          input = Sample2.nat;
          assertion = [ (0, true); (1, false); (2, true); (3, false) ];
          func =
            (let rec f : int -> bool =
              fun x ->
               if x = 0 then
                 true
               else if x = 1 then
                 false
               else
                 f (x - 2)
             in
             f);
          background = [];
        } );
    ( "nat_max",
      proj
        {
          function_name = "nat_max";
          k_max = 15;
          d_in = Denotation.args2 Denotation.int Denotation.int;
          d_out = Denotation.int;
          input = Sample2.pair Sample2.nat Sample2.nat;
          assertion =
            [
              ((0, 0), 0);
              ((2, 2), 2);
              ((0, 3), 3);
              ((2, 1), 2);
              ((3, 0), 3);
              ((0, 1), 1);
              ((1, 0), 1);
              ((3, 2), 3);
              ((0, 2), 2);
              ((1, 1), 1);
              ((3, 1), 3);
              ((2, 0), 2);
              ((2, 3), 3);
              ((1, 2), 2);
              ((3, 3), 3);
            ];
          func =
            (let f : int * int -> int =
              fun (x, y) ->
               if x >= y then
                 x
               else
                 y
             in
             f);
          background = [];
        } );
    ( "nat_pred",
      proj
        {
          function_name = "nat_pred";
          k_max = 4;
          d_in = Denotation.int;
          d_out = Denotation.int;
          input = Sample2.nat;
          assertion = [ (0, 0); (1, 0); (3, 2); (2, 1) ];
          func =
            (let f : int -> int =
              fun x ->
               if x = 0 then
                 0
               else
                 x - 1
             in
             f);
          background = [];
        } );
    ( "nat_add",
      proj
        {
          function_name = "nat_add";
          k_max = 9;
          d_in = Denotation.args2 Denotation.int Denotation.int;
          d_out = Denotation.int;
          input = Sample2.pair Sample2.nat Sample2.nat;
          assertion =
            [
              ((0, 0), 0);
              ((2, 2), 4);
              ((3, 0), 3);
              ((2, 1), 3);
              ((3, 3), 6);
              ((1, 2), 3);
              ((0, 2), 2);
              ((1, 3), 4);
              ((3, 2), 5);
            ];
          func =
            (let f : int * int -> int = fun (x, y) -> x + y in
             f);
          background = [];
        } );
    ( "list_append",
      proj
        {
          function_name = "list_append";
          k_max = 20;
          d_in =
            Denotation.args2
              (Denotation.list Denotation.int)
              (Denotation.list Denotation.int);
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat_list;
          assertion =
            [
              (([ 3; 0; 1; 2 ], [ 0 ]), [ 3; 0; 1; 2; 0 ]);
              (([], []), []);
              (([ 1; 2 ], [ 3; 2; 1 ]), [ 1; 2; 3; 2; 1 ]);
              (([ 2 ], [ 0; 1; 1 ]), [ 2; 0; 1; 1 ]);
              (([ 2; 1 ], [ 0 ]), [ 2; 1; 0 ]);
              (([], [ 3; 2 ]), [ 3; 2 ]);
              (([], [ 2; 2; 3 ]), [ 2; 2; 3 ]);
              (([ 0; 3; 3; 0 ], [ 2; 2 ]), [ 0; 3; 3; 0; 2; 2 ]);
              (([], [ 0; 2; 3 ]), [ 0; 2; 3 ]);
              (([ 0 ], [ 3; 3; 1; 3 ]), [ 0; 3; 3; 1; 3 ]);
              (([ 0; 2; 1; 3 ], [ 3; 0 ]), [ 0; 2; 1; 3; 3; 0 ]);
              (([ 3; 2; 2; 1 ], [ 1 ]), [ 3; 2; 2; 1; 1 ]);
              (([ 0; 0; 3 ], [ 1; 1 ]), [ 0; 0; 3; 1; 1 ]);
              (([ 2; 3; 0 ], []), [ 2; 3; 0 ]);
              (([], [ 2; 0 ]), [ 2; 0 ]);
              (([ 0; 2; 3 ], []), [ 0; 2; 3 ]);
              (([ 1; 1 ], [ 2 ]), [ 1; 1; 2 ]);
              (([ 2; 0; 0 ], [ 3 ]), [ 2; 0; 0; 3 ]);
              (([], [ 2; 1; 0; 2 ]), [ 2; 1; 0; 2 ]);
              (([], [ 3 ]), [ 3 ]);
            ];
          func =
            (let f : int list * int list -> int list =
              fun (xs, ys) -> xs @ ys
             in
             f);
          background = [];
        } );
    ( "list_compress",
      proj
        {
          function_name = "list_compress";
          k_max = 20;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 1; 2; 3; 1 ], [ 1; 2; 3; 1 ]);
              ([], []);
              ([ 0 ], [ 0 ]);
              ([ 3; 3; 3 ], [ 3 ]);
              ([ 3 ], [ 3 ]);
              ([ 3; 3; 2; 2 ], [ 3; 2 ]);
              ([ 2; 0; 2; 2 ], [ 2; 0; 2 ]);
              ([ 2; 1; 2; 1 ], [ 2; 1; 2; 1 ]);
              ([ 3; 3 ], [ 3 ]);
              ([ 1 ], [ 1 ]);
              ([ 2 ], [ 2 ]);
              ([ 1; 2; 3 ], [ 1; 2; 3 ]);
              ([ 0; 1; 3 ], [ 0; 1; 3 ]);
              ([ 2; 2 ], [ 2 ]);
              ([ 0; 0 ], [ 0 ]);
              ([ 3; 2; 0; 3 ], [ 3; 2; 0; 3 ]);
              ([ 1; 0; 2 ], [ 1; 0; 2 ]);
              ([ 2; 1 ], [ 2; 1 ]);
              ([ 2; 2; 1 ], [ 2; 1 ]);
              ([ 1; 1; 2 ], [ 1; 2 ]);
            ];
          func =
            (let f : int list -> int list = fun xs -> list_compress xs in
             f);
          background = [ "less_than"; "equal_to" ];
        } );
    ( "list_concat",
      proj
        {
          function_name = "list_concat";
          k_max = 20;
          d_in = Denotation.nested_list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nested_nat_list;
          assertion =
            [
              ([ [ 0; 1 ]; [ 3; 2 ]; [ 3 ]; [ 0 ] ], [ 0; 1; 3; 2; 3; 0 ]);
              ([ [] ], []);
              ([ [ 2 ]; [ 3 ]; [ 1; 1 ] ], [ 2; 3; 1; 1 ]);
              ([ [ 3; 1 ]; [] ], [ 3; 1 ]);
              ([ [] ], []);
              ([ []; []; [ 2 ]; [ 1 ] ], [ 2; 1 ]);
              ([ [ 1; 2 ]; [ 0; 1 ]; [ 1 ] ], [ 1; 2; 0; 1; 1 ]);
              ([ [ 0 ]; [ 2 ]; [ 0 ] ], [ 0; 2; 0 ]);
              ([ []; [ 3 ] ], [ 3 ]);
              ([ [ 1 ]; [] ], [ 1 ]);
              ([ [ 1 ]; [ 2; 2 ]; []; [] ], [ 1; 2; 2 ]);
              ([ []; []; [ 0; 2 ]; [ 0 ] ], [ 0; 2; 0 ]);
              ([ [ 0; 3 ]; [ 1; 0 ]; [ 3 ]; [ 2 ] ], [ 0; 3; 1; 0; 3; 2 ]);
              ([ [ 3 ] ], [ 3 ]);
              ([ [ 0 ]; [ 0; 0 ] ], [ 0; 0; 0 ]);
              ([ [ 2 ]; [ 3 ]; [ 0; 0 ]; [ 2; 3 ] ], [ 2; 3; 0; 0; 2; 3 ]);
              ([ [ 0 ]; []; [ 3; 3 ]; [ 2; 0 ] ], [ 0; 3; 3; 2; 0 ]);
              ([ [ 0 ]; []; [ 1 ]; [ 1; 2 ] ], [ 0; 1; 1; 2 ]);
              ([ [ 3; 1 ]; [ 0; 3 ] ], [ 3; 1; 0; 3 ]);
              ([ [ 2 ]; [ 3; 1 ]; [ 3; 2 ]; [] ], [ 2; 3; 1; 3; 2 ]);
            ];
          func =
            (let f : int list list -> int list = fun xss -> List.concat xss in
             f);
          background = [ "append" ];
        } );
    ( "list_drop",
      proj
        {
          function_name = "list_drop";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.list Denotation.int) Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat;
          assertion =
            [
              (([ 0; 0 ], 0), [ 0; 0 ]);
              (([], 0), []);
              (([ 3; 2; 1 ], 0), [ 3; 2; 1 ]);
              (([], 3), []);
              (([], 2), []);
              (([ 3; 3 ], 2), []);
              (([ 1; 0; 1; 1 ], 0), [ 1; 0; 1; 1 ]);
              (([ 3; 3 ], 3), []);
              (([ 3; 0; 1 ], 3), []);
              (([ 3; 1 ], 2), []);
              (([ 1; 0; 0; 0 ], 2), [ 0; 0 ]);
              (([ 0; 2; 1; 1 ], 0), [ 0; 2; 1; 1 ]);
              (([ 0; 2; 3 ], 1), [ 2; 3 ]);
              (([ 0 ], 3), []);
              (([ 2; 2; 3 ], 2), [ 3 ]);
              (([ 3; 3; 0 ], 2), [ 0 ]);
              (([ 0; 3 ], 3), []);
              (([], 1), []);
              (([ 3; 1; 0; 0 ], 0), [ 3; 1; 0; 0 ]);
              (([ 3; 1; 3 ], 2), [ 3 ]);
            ];
          func =
            (let f : int list * int -> int list =
              fun (xs, n) -> List.drop xs n
             in
             f);
          background = [];
        } );
    ( "list_even_parity",
      proj
        {
          function_name = "list_even_parity";
          k_max = 15;
          d_in = Denotation.list Denotation.bool;
          d_out = Denotation.bool;
          input = Sample2.bool_list;
          assertion =
            [
              ([ false; true ], false);
              ([], true);
              ([ false ], true);
              ([ false; false ], true);
              ([ true ], false);
              ([ true; true; false ], true);
              ([ true; true ], true);
              ([ true; false; false; true ], true);
              ([ false; true; true; true ], false);
              ([ false; true; true ], true);
              ([ true; false; true; true ], false);
              ([ false; false; false; false ], true);
              ([ true; true; true; false ], false);
              ([ true; false; true ], true);
              ([ false; true; false ], false);
            ];
          func =
            (let f : bool list -> bool = fun bs -> list_even_parity bs in
             f);
          background = [ "not" ];
        } );
    ( "list_filter",
      proj
        {
          function_name = "list_filter";
          k_max = 20;
          d_in =
            Denotation.args2 Denotation.var (Denotation.list Denotation.int);
          d_out = Denotation.list Denotation.int;
          input =
            Sample2.pair
              (Sample2.from ("isEven", [ "isNonzero" ]))
              Sample2.nat_list;
          assertion =
            [
              (("isEven", [ 1; 2; 0 ]), [ 2; 0 ]);
              (("isEven", []), []);
              (("isNonzero", [ 3 ]), [ 3 ]);
              (("isNonzero", [ 2; 2; 1 ]), [ 2; 2; 1 ]);
              (("isNonzero", []), []);
              (("isEven", [ 2 ]), [ 2 ]);
              (("isEven", [ 3; 3 ]), []);
              (("isNonzero", [ 0; 0; 3; 1 ]), [ 3; 1 ]);
              (("isNonzero", [ 2; 3; 1 ]), [ 2; 3; 1 ]);
              (("isEven", [ 1 ]), []);
              (("isEven", [ 3; 1; 3; 2 ]), [ 2 ]);
              (("isEven", [ 0; 1 ]), [ 0 ]);
              (("isNonzero", [ 0 ]), []);
              (("isNonzero", [ 1; 1; 1 ]), [ 1; 1; 1 ]);
              (("isEven", [ 0 ]), [ 0 ]);
              (("isNonzero", [ 1 ]), [ 1 ]);
              (("isEven", [ 2; 3; 0 ]), [ 2; 0 ]);
              (("isNonzero", [ 3; 0; 0 ]), [ 3 ]);
              (("isNonzero", [ 2 ]), [ 2 ]);
              (("isEven", [ 3 ]), []);
            ];
          func =
            (let f : string * int list -> int list =
              fun (fname, xs) ->
               let pred =
                 match fname with
                 | "isEven" -> fun x -> x mod 2 = 0
                 | "isNonzero" -> fun x -> x <> 0
                 | _ -> failwith ("Unknown Myth built-in '" ^ fname ^ "'")
               in
               List.filter ~f:pred xs
             in
             f);
          background = [ "isEven"; "isNonzero" ];
        } );
    ( "list_fold",
      proj
        {
          function_name = "list_fold";
          k_max = 20;
          d_in =
            Denotation.args3 Denotation.var Denotation.int
              (Denotation.list Denotation.int);
          d_out = Denotation.int;
          input =
            Sample2.triple
              (Sample2.from ("add", [ "countOdd" ]))
              Sample2.nat Sample2.nat_list;
          assertion =
            [
              (("countOdd", 2, [ 2 ]), 2);
              (("add", 0, []), 0);
              (("add", 3, [ 1; 0; 2; 3 ]), 9);
              (("countOdd", 2, [ 0; 1 ]), 3);
              (("countOdd", 3, [ 0; 2 ]), 3);
              (("countOdd", 2, [ 3 ]), 3);
              (("countOdd", 1, [ 1; 1; 2; 2 ]), 3);
              (("add", 2, [ 0; 3; 2; 3 ]), 10);
              (("add", 1, [ 3 ]), 4);
              (("countOdd", 2, [ 0; 3 ]), 3);
              (("countOdd", 3, []), 3);
              (("countOdd", 3, [ 3; 2; 1; 0 ]), 5);
              (("add", 0, [ 2; 1; 1; 1 ]), 5);
              (("add", 3, [ 3; 2 ]), 8);
              (("countOdd", 3, [ 3 ]), 4);
              (("add", 2, [ 2; 3; 2; 0 ]), 9);
              (("add", 2, [ 1; 3; 3; 0 ]), 9);
              (("countOdd", 2, [ 0 ]), 2);
              (("countOdd", 0, [ 3; 3; 0; 0 ]), 2);
              (("add", 2, []), 2);
            ];
          func =
            (let f : string * int * int list -> int =
              fun (fname, acc, xs) ->
               let folder =
                 match fname with
                 | "add" -> fun x y -> x + y
                 | "countOdd" ->
                     fun a x ->
                       if x mod 2 = 1 then
                         a + 1
                       else
                         a
                 | _ -> failwith ("Unknown Myth built-in '" ^ fname ^ "'")
               in
               List.fold_left ~f:folder ~init:acc xs
             in
             f);
          background = [ "countOdd"; "add" ];
        } );
    ( "list_hd",
      proj
        {
          function_name = "list_hd";
          k_max = 10;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 0; 3; 3; 0 ], 0);
              ([], 0);
              ([ 0; 1; 3 ], 0);
              ([ 0; 3; 2 ], 0);
              ([ 2; 1 ], 2);
              ([ 1; 3; 2 ], 1);
              ([ 2 ], 2);
              ([ 0; 3 ], 0);
              ([ 2; 3; 0; 2 ], 2);
              ([ 0; 0; 3; 2 ], 0);
            ];
          func =
            (let f : int list -> int =
              fun xs ->
               match xs with
               | [] -> 0
               | head :: _ -> head
             in
             f);
          background = [];
        } );
    ( "list_inc",
      proj
        {
          function_name = "list_inc";
          k_max = 10;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 2; 0 ], [ 3; 1 ]);
              ([], []);
              ([ 0; 3; 2; 2 ], [ 1; 4; 3; 3 ]);
              ([ 3 ], [ 4 ]);
              ([ 0 ], [ 1 ]);
              ([ 2 ], [ 3 ]);
              ([ 1; 0 ], [ 2; 1 ]);
              ([ 1; 3; 1 ], [ 2; 4; 2 ]);
              ([ 3; 0; 1; 3 ], [ 4; 1; 2; 4 ]);
              ([ 2; 3; 3; 2 ], [ 3; 4; 4; 3 ]);
            ];
          func =
            (let f : int list -> int list =
              fun xs -> List.map ~f:(( + ) 1) xs
             in
             f);
          background = [ "map"; "inc" ];
        } );
    ( "list_last",
      proj
        {
          function_name = "list_last";
          k_max = 20;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.opt Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 3; 2; 3 ], Some 3);
              ([], None);
              ([ 1; 3 ], Some 3);
              ([ 0 ], Some 0);
              ([ 0; 3 ], Some 3);
              ([ 3 ], Some 3);
              ([ 2; 3; 0; 3 ], Some 3);
              ([ 0; 2; 1; 1 ], Some 1);
              ([ 1; 0; 1 ], Some 1);
              ([ 0; 1 ], Some 1);
              ([ 3; 0; 0 ], Some 0);
              ([ 2 ], Some 2);
              ([ 0; 3; 3; 2 ], Some 2);
              ([ 0; 3; 1 ], Some 1);
              ([ 3; 1 ], Some 1);
              ([ 3; 0; 0; 1 ], Some 1);
              ([ 0; 0 ], Some 0);
              ([ 1 ], Some 1);
              ([ 3; 1; 1 ], Some 1);
              ([ 2; 0; 2 ], Some 2);
           ];
          func =
            (let rec f : int list -> int option =
              fun xs ->
               match xs with
               | [] -> None
               | [ x ] -> Some x
               | _ :: tail -> f tail
             in
             f);
          background = [];
        } );
    ( "list_length",
      proj
        {
          function_name = "list_length";
          k_max = 5;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 0; 3; 1; 0 ], 4);
              ([], 0);
              ([ 3; 3; 2; 3 ], 4);
              ([ 2; 2; 0 ], 3);
              ([ 3 ], 1);
            ];
          func =
            (let f : int list -> int = fun xs -> List.length xs in
             f);
          background = [];
        } );
    ( "list_map",
      proj
        {
          function_name = "list_map";
          k_max = 20;
          d_in =
            Denotation.args2 Denotation.var (Denotation.list Denotation.int);
          d_out = Denotation.list Denotation.int;
          input =
            Sample2.pair (Sample2.from ("inc", [ "zero" ])) Sample2.nat_list;
          assertion =
            [
              (("zero", [ 1 ]), [ 0 ]);
              (("inc", []), []);
              (("zero", []), []);
              (("zero", [ 0; 1; 1 ]), [ 0; 0; 0 ]);
              (("zero", [ 0; 0; 0 ]), [ 0; 0; 0 ]);
              (("zero", [ 0; 2; 2; 0 ]), [ 0; 0; 0; 0 ]);
              (("zero", [ 2; 0; 2; 0 ]), [ 0; 0; 0; 0 ]);
              (("zero", [ 0; 0; 1; 0 ]), [ 0; 0; 0; 0 ]);
              (("zero", [ 0; 1; 3; 2 ]), [ 0; 0; 0; 0 ]);
              (("zero", [ 0; 0; 1; 3 ]), [ 0; 0; 0; 0 ]);
              (("inc", [ 1 ]), [ 2 ]);
              (("inc", [ 2 ]), [ 3 ]);
              (("inc", [ 0; 2; 1; 3 ]), [ 1; 3; 2; 4 ]);
              (("inc", [ 0; 1; 2; 3 ]), [ 1; 2; 3; 4 ]);
              (("inc", [ 2; 3; 1; 3 ]), [ 3; 4; 2; 4 ]);
              (("inc", [ 2; 0; 2 ]), [ 3; 1; 3 ]);
              (("inc", [ 0; 1; 1 ]), [ 1; 2; 2 ]);
              (("inc", [ 0; 2 ]), [ 1; 3 ]);
              (("inc", [ 3; 0 ]), [ 4; 1 ]);
              (("inc", [ 3; 2 ]), [ 4; 3 ]);
            ];
          func =
            (let f : string * int list -> int list =
              fun (fname, xs) ->
               let mapper =
                 match fname with
                 | "inc" -> fun x -> x + 1
                 | "zero" -> fun _ -> 0
                 | _ -> failwith ("Unknown Myth built-in '" ^ fname ^ "'")
               in
               List.map ~f:mapper xs
             in
             f);
          background = [ "inc"; "zero" ];
        } );
    ( "list_nth",
      proj
        {
          function_name = "list_nth";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.list Denotation.int) Denotation.int;
          d_out = Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat;
          assertion =
            [
              (([ 2 ], 0), 2);
              (([], 0), 0);
              (([ 3; 1; 2 ], 1), 1);
              (([ 2; 3 ], 1), 3);
              (([ 0; 2; 0 ], 3), 0);
              (([ 1; 0 ], 2), 0);
              (([ 3; 3; 1 ], 3), 0);
              (([], 1), 0);
              (([ 3; 3 ], 0), 3);
              (([ 1; 3; 3; 1 ], 3), 1);
              (([ 2 ], 3), 0);
              (([ 3; 0 ], 0), 3);
              (([ 0; 3 ], 0), 0);
              (([ 0; 0; 2; 0 ], 1), 0);
              (([ 0; 2; 1; 1 ], 2), 1);
              (([ 3; 0; 2; 2 ], 3), 2);
              (([], 2), 0);
              (([ 3; 3; 3 ], 1), 3);
              (([ 2; 1; 2 ], 1), 1);
              (([ 3; 1; 1 ], 2), 1);
            ];
          func =
            (let rec f : int list * int -> int =
              fun (xs, n) ->
               match xs with
               | [] -> 0
               | head :: tail ->
                   if n = 0 then
                     head
                   else
                     f (tail, n - 1)
             in
             f);
          background = [];
        } );
    ( "list_pairwise_swap",
      proj
        {
          function_name = "list_pairwise_swap";
          k_max = 20;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 2; 0 ], [ 0; 2 ]);
              ([], []);
              ([ 0 ], []);
              ([ 3; 0; 1 ], []);
              ([ 1; 2; 1 ], []);
              ([ 1; 0; 0; 0 ], [ 0; 1; 0; 0 ]);
              ([ 0; 3 ], [ 3; 0 ]);
              ([ 0; 3; 0; 2 ], [ 3; 0; 2; 0 ]);
              ([ 3; 1; 3; 0 ], [ 1; 3; 0; 3 ]);
              ([ 1 ], []);
              ([ 3; 0; 0 ], []);
              ([ 0; 1; 2 ], []);
              ([ 1; 3 ], [ 3; 1 ]);
              ([ 3 ], []);
              ([ 2 ], []);
              ([ 3; 0; 3 ], []);
              ([ 1; 2; 2; 0 ], [ 2; 1; 0; 2 ]);
              ([ 2; 0; 3 ], []);
              ([ 2; 1; 1 ], []);
              ([ 1; 0; 0 ], []);
              (* added to avoid false positive *)
              ([ 1; 2; 3; 0; 1 ], []);
              ([ 1; 2; 3; 0; 1; 4; 2 ], []);
            ];
          func =
            (let f : int list -> int list =
              fun xs ->
               match list_pairwise_swap xs with
               | None -> []
               | Some flipped -> flipped
             in
             f);
          background = [];
        } );
    ( "list_rev_append",
      proj
        {
          function_name = "list_rev_append";
          k_max = 15;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 3 ], [ 3 ]);
              ([], []);
              ([ 2; 3 ], [ 3; 2 ]);
              ([ 2; 0; 3; 3 ], [ 3; 3; 0; 2 ]);
              ([ 3; 3; 0 ], [ 0; 3; 3 ]);
              ([ 3; 0; 1 ], [ 1; 0; 3 ]);
              ([ 1; 2 ], [ 2; 1 ]);
              ([ 1; 1; 1 ], [ 1; 1; 1 ]);
              ([ 0 ], [ 0 ]);
              ([ 1; 3; 2 ], [ 2; 3; 1 ]);
              ([ 0; 1; 3; 3 ], [ 3; 3; 1; 0 ]);
              ([ 1; 3; 3; 1 ], [ 1; 3; 3; 1 ]);
              ([ 1; 1; 3 ], [ 3; 1; 1 ]);
              ([ 3; 3 ], [ 3; 3 ]);
              ([ 3; 0; 2; 0 ], [ 0; 2; 0; 3 ]);
            ];
          func =
            (let f : int list -> int list = fun xs -> List.rev xs in
             f);
          background = [ "append" ];
        } );
    ( "list_rev_fold",
      proj
        {
          function_name = "list_rev_fold";
          k_max = 15;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 1 ], [ 1 ]);
              ([], []);
              ([ 0; 1; 0 ], [ 0; 1; 0 ]);
              ([ 3; 0; 0; 2 ], [ 2; 0; 0; 3 ]);
              ([ 3 ], [ 3 ]);
              ([ 2; 0; 1; 2 ], [ 2; 1; 0; 2 ]);
              ([ 2; 0; 0; 2 ], [ 2; 0; 0; 2 ]);
              ([ 3; 1 ], [ 1; 3 ]);
              ([ 1; 2 ], [ 2; 1 ]);
              ([ 0; 2; 1; 3 ], [ 3; 1; 2; 0 ]);
              ([ 0 ], [ 0 ]);
              ([ 0; 2 ], [ 2; 0 ]);
              ([ 1; 3 ], [ 3; 1 ]);
              ([ 3; 2 ], [ 2; 3 ]);
              ([ 2 ], [ 2 ]);
            ];
          func =
            (let f : int list -> int list = fun xs -> List.rev xs in
             f);
          background = [ "snoc"; "fold" ];
        } );
    ( "list_rev_snoc",
      proj
        {
          function_name = "list_rev_snoc";
          k_max = 15;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 2; 1; 0 ], [ 0; 1; 2 ]);
              ([], []);
              ([ 2; 2; 0 ], [ 0; 2; 2 ]);
              ([ 2; 3; 3 ], [ 3; 3; 2 ]);
              ([ 2; 0 ], [ 0; 2 ]);
              ([ 2 ], [ 2 ]);
              ([ 2; 1; 3; 1 ], [ 1; 3; 1; 2 ]);
              ([ 0; 2 ], [ 2; 0 ]);
              ([ 2; 1; 3 ], [ 3; 1; 2 ]);
              ([ 2; 2 ], [ 2; 2 ]);
              ([ 3; 3; 0 ], [ 0; 3; 3 ]);
              ([ 0 ], [ 0 ]);
              ([ 0; 1 ], [ 1; 0 ]);
              ([ 0; 3; 1; 0 ], [ 0; 1; 3; 0 ]);
              ([ 1; 0; 0 ], [ 0; 0; 1 ]);
            ];
          func =
            (let f : int list -> int list = fun xs -> List.rev xs in
             f);
          background = [ "snoc" ];
        } );
    ( "list_rev_tailcall",
      proj
        {
          function_name = "list_rev_tailcall";
          k_max = 20;
          d_in =
            Denotation.args2
              (Denotation.list Denotation.int)
              (Denotation.list Denotation.int);
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat_list;
          assertion =
            [
              (([ 2; 2; 2; 2 ], [ 1; 3; 1 ]), [ 2; 2; 2; 2; 1; 3; 1 ]);
              (([], []), []);
              (([ 1; 1; 2 ], []), [ 2; 1; 1 ]);
              (([ 2; 3; 3 ], [ 3; 3 ]), [ 3; 3; 2; 3; 3 ]);
              (([], [ 2; 0; 3; 2 ]), [ 2; 0; 3; 2 ]);
              (([ 2; 2 ], [ 0; 1; 2 ]), [ 2; 2; 0; 1; 2 ]);
              (([ 0; 3; 1; 2 ], []), [ 2; 1; 3; 0 ]);
              (([ 1; 2 ], [ 1; 0; 2 ]), [ 2; 1; 1; 0; 2 ]);
              (([], [ 2 ]), [ 2 ]);
              (([ 1; 1 ], [ 2 ]), [ 1; 1; 2 ]);
              (([ 1 ], [ 3 ]), [ 1; 3 ]);
              (([ 3 ], [ 0 ]), [ 3; 0 ]);
              (([ 1; 2; 0; 2 ], []), [ 2; 0; 2; 1 ]);
              (([ 2; 0; 2 ], []), [ 2; 0; 2 ]);
              (([ 0; 1; 1; 0 ], [ 1; 0 ]), [ 0; 1; 1; 0; 1; 0 ]);
              (([], [ 0; 3; 2 ]), [ 0; 3; 2 ]);
              (([ 0 ], [ 3; 2; 1 ]), [ 0; 3; 2; 1 ]);
              (([ 2; 3 ], [ 0 ]), [ 3; 2; 0 ]);
              (([ 3; 3; 2; 3 ], [ 1; 0 ]), [ 3; 2; 3; 3; 1; 0 ]);
              (([ 2 ], []), [ 2 ]);
            ];
          func =
            (let rec f : int list * int list -> int list =
              fun (xs, acc) ->
               match xs with
               | [] -> acc
               | head :: tail -> f (tail, head :: acc)
             in
             f);
          background = [];
        } );
    ( "list_snoc",
      proj
        {
          function_name = "list_snoc";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.list Denotation.int) Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat;
          assertion =
            [
              (([ 1 ], 0), [ 1; 0 ]);
              (([], 0), [ 0 ]);
              (([], 3), [ 3 ]);
              (([], 1), [ 1 ]);
              (([ 1 ], 2), [ 1; 2 ]);
              (([ 1; 3; 3 ], 0), [ 1; 3; 3; 0 ]);
              (([ 2; 3; 1 ], 3), [ 2; 3; 1; 3 ]);
              (([ 2; 0 ], 2), [ 2; 0; 2 ]);
              (([ 3; 0 ], 3), [ 3; 0; 3 ]);
              (([ 0; 3; 0; 0 ], 0), [ 0; 3; 0; 0; 0 ]);
              (([], 2), [ 2 ]);
              (([ 2; 2; 3 ], 0), [ 2; 2; 3; 0 ]);
              (([ 0 ], 3), [ 0; 3 ]);
              (([ 0; 2; 3; 1 ], 1), [ 0; 2; 3; 1; 1 ]);
              (([ 0; 3 ], 0), [ 0; 3; 0 ]);
              (([ 1; 1; 3; 2 ], 2), [ 1; 1; 3; 2; 2 ]);
              (([ 1; 0; 2 ], 0), [ 1; 0; 2; 0 ]);
              (([ 3; 0; 3; 2 ], 2), [ 3; 0; 3; 2; 2 ]);
              (([ 0 ], 0), [ 0; 0 ]);
              (([ 2; 2; 1; 1 ], 1), [ 2; 2; 1; 1; 1 ]);
            ];
          func =
            (let f : int list * int -> int list = fun (xs, n) -> xs @ [ n ] in
             f);
          background = [];
        } );
    ( "list_sort_sorted_insert",
      proj
        {
          function_name = "list_sort_sorted_insert";
          k_max = 20;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 3 ], [ 3 ]);
              ([], []);
              ([ 1; 3 ], [ 1; 3 ]);
              ([ 1; 1 ], [ 1 ]);
              ([ 0; 1; 1 ], [ 0; 1 ]);
              ([ 3; 1; 1 ], [ 1; 3 ]);
              ([ 2; 2; 0 ], [ 0; 2 ]);
              ([ 3; 0; 2 ], [ 0; 2; 3 ]);
              ([ 0 ], [ 0 ]);
              ([ 0; 1; 1; 0 ], [ 0; 1 ]);
              ([ 2; 2; 0; 2 ], [ 0; 2 ]);
              ([ 3; 2; 2; 0 ], [ 0; 2; 3 ]);
              ([ 1; 2 ], [ 1; 2 ]);
              ([ 1 ], [ 1 ]);
              ([ 0; 3; 3; 2 ], [ 0; 2; 3 ]);
              ([ 3; 3; 3; 2 ], [ 2; 3 ]);
              ([ 2 ], [ 2 ]);
              ([ 2; 1; 1; 1 ], [ 1; 2 ]);
              ([ 3; 2; 2 ], [ 2; 3 ]);
              ([ 2; 2; 3 ], [ 2; 3 ]);
            ];
          func =
            (let f : int list -> int list =
              fun xs -> List.dedup_and_sort ~compare:Int.compare xs
             in
             f);
          background = [ "insert" ];
        } );
    ( "list_sorted_insert",
      proj
        {
          function_name = "list_sorted_insert";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.list Denotation.int) Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat_list Sample2.nat;
          assertion =
            [
              (([ 2 ], 0), [ 0; 2 ]);
              (([], 0), [ 0 ]);
              (([ 2; 0 ], 3), [ 2; 0; 3 ]);
              (([ 3 ], 3), [ 3 ]);
              (([ 0; 0 ], 1), [ 0; 0; 1 ]);
              (([ 2; 3; 0; 3 ], 2), [ 2; 3; 0; 3 ]);
              (([ 3; 1; 2 ], 2), [ 2; 3; 1; 2 ]);
              (([], 2), [ 2 ]);
              (([], 3), [ 3 ]);
              (([ 0; 1; 1; 1 ], 1), [ 0; 1; 1; 1 ]);
              (([], 1), [ 1 ]);
              (([ 3; 3; 1; 3 ], 0), [ 0; 3; 3; 1; 3 ]);
              (([ 1; 3; 3; 1 ], 2), [ 1; 2; 3; 3; 1 ]);
              (([ 0; 3 ], 1), [ 0; 1; 3 ]);
              (([ 3; 1 ], 2), [ 2; 3; 1 ]);
              (([ 2; 3; 1 ], 0), [ 0; 2; 3; 1 ]);
              (([ 2; 2; 1 ], 0), [ 0; 2; 2; 1 ]);
              (([ 1 ], 0), [ 0; 1 ]);
              (([ 3 ], 0), [ 0; 3 ]);
              (([ 3; 3; 0; 3 ], 3), [ 3; 3; 0; 3 ]);
            ];
          func =
            (let f : int list * int -> int list =
              fun (xs, n) -> list_sorted_insert n xs
             in
             f);
          background = [ "less_than"; "equal_to" ];
        } );
    ( "list_stutter",
      proj
        {
          function_name = "list_stutter";
          k_max = 10;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 2; 3; 1; 2 ], [ 2; 2; 3; 3; 1; 1; 2; 2 ]);
              ([], []);
              ([ 3; 2 ], [ 3; 3; 2; 2 ]);
              ([ 3; 3; 2; 2 ], [ 3; 3; 3; 3; 2; 2; 2; 2 ]);
              ([ 3; 0; 3; 3 ], [ 3; 3; 0; 0; 3; 3; 3; 3 ]);
              ([ 3 ], [ 3; 3 ]);
              ([ 1; 2 ], [ 1; 1; 2; 2 ]);
              ([ 2; 0; 2; 3 ], [ 2; 2; 0; 0; 2; 2; 3; 3 ]);
              ([ 1; 1 ], [ 1; 1; 1; 1 ]);
              ([ 1 ], [ 1; 1 ]);
            ];
          func =
            (let rec f : int list -> int list =
              fun xs ->
               match xs with
               | [] -> []
               | y :: ys -> y :: y :: f ys
             in
             f);
          background = [];
        } );
    ( "list_sum",
      proj
        {
          function_name = "list_sum";
          k_max = 10;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 0; 0; 3; 3 ], 6);
              ([], 0);
              ([ 3; 1 ], 4);
              ([ 3; 2 ], 5);
              ([ 2; 3; 0 ], 5);
              ([ 2; 3; 0; 2 ], 7);
              ([ 2 ], 2);
              ([ 3; 0 ], 3);
              ([ 0 ], 0);
              ([ 0; 2; 1 ], 3);
            ];
          func =
            (let f : int list -> int =
              fun xs -> List.fold ~init:0 ~f:( + ) xs
             in
             f);
          background = [ "fold"; "add" ];
        } );
    ( "list_take",
      proj
        {
          function_name = "list_take";
          k_max = 20;
          d_in =
            Denotation.args2 Denotation.int (Denotation.list Denotation.int);
          d_out = Denotation.list Denotation.int;
          input = Sample2.pair Sample2.nat Sample2.nat_list;
          assertion =
            [
              ((2, [ 3 ]), [ 3 ]);
              ((0, []), []);
              ((1, []), []);
              ((3, []), []);
              ((2, []), []);
              ((2, [ 3; 3; 1 ]), [ 3; 3 ]);
              ((0, [ 1 ]), []);
              ((2, [ 0; 3 ]), [ 0; 3 ]);
              ((2, [ 3; 2; 0 ]), [ 3; 2 ]);
              ((2, [ 3; 0; 2; 2 ]), [ 3; 0 ]);
              ((1, [ 2 ]), [ 2 ]);
              ((2, [ 0; 3; 3; 1 ]), [ 0; 3 ]);
              ((3, [ 3; 1; 1; 0 ]), [ 3; 1; 1 ]);
              ((2, [ 1; 3; 2; 2 ]), [ 1; 3 ]);
              ((1, [ 0; 1; 1 ]), [ 0 ]);
              ((1, [ 3; 2; 0; 3 ]), [ 3 ]);
              ((3, [ 2; 3 ]), [ 2; 3 ]);
              ((3, [ 1; 2; 0; 2 ]), [ 1; 2; 0 ]);
              ((1, [ 2; 0; 1 ]), [ 2 ]);
              ((0, [ 2 ]), []);
            ];
          func =
            (let f : int * int list -> int list =
              fun (n, xs) -> List.take xs n
             in
             f);
          background = [];
        } );
    ( "list_tl",
      proj
        {
          function_name = "list_tl";
          k_max = 10;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_list;
          assertion =
            [
              ([ 0; 3 ], [ 3 ]);
              ([], []);
              ([ 2 ], []);
              ([ 1 ], []);
              ([ 3; 0; 1; 0 ], [ 0; 1; 0 ]);
              ([ 2; 2; 2 ], [ 2; 2 ]);
              ([ 3; 3; 2 ], [ 3; 2 ]);
              ([ 0; 0; 2 ], [ 0; 2 ]);
              ([ 2; 2; 0; 0 ], [ 2; 0; 0 ]);
              ([ 3; 1; 1 ], [ 1; 1 ]);
            ];
          func =
            (let f : int list -> int list =
              fun xs ->
               match xs with
               | [] -> []
               | _ :: tail -> tail
             in
             f);
          background = [];
        } );
    ( "tree_binsert",
      proj
        {
          function_name = "tree_binsert";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.tree Denotation.int) Denotation.int;
          d_out = Denotation.tree Denotation.int;
          input = Sample2.pair Sample2.nat_tree Sample2.nat;
          func =
            (let f : int Tree2.t * int -> int Tree2.t =
              fun (tree, y) -> Tree2.binsert y tree
             in
             f);
          assertion =
            [
              ( (Node (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 0, Leaf), 2),
                Node
                  (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 0, Node (Leaf, 2, Leaf))
              );
              ((Leaf, 0), Node (Leaf, 0, Leaf));
              ( (Node (Node (Leaf, 0, Leaf), 1, Leaf), 1),
                Node (Node (Leaf, 0, Leaf), 1, Leaf) );
              ( ( Node
                    ( Leaf,
                      3,
                      Node (Node (Leaf, 3, Node (Leaf, 1, Leaf)), 3, Leaf) ),
                  1 ),
                Node
                  ( Node (Leaf, 1, Leaf),
                    3,
                    Node (Node (Leaf, 3, Node (Leaf, 1, Leaf)), 3, Leaf) ) );
              ( ( Node
                    ( Node (Node (Node (Leaf, 0, Leaf), 0, Leaf), 1, Leaf),
                      0,
                      Leaf ),
                  1 ),
                Node
                  ( Node (Node (Node (Leaf, 0, Leaf), 0, Leaf), 1, Leaf),
                    0,
                    Node (Leaf, 1, Leaf) ) );
              ( (Node (Leaf, 0, Node (Leaf, 2, Leaf)), 2),
                Node (Leaf, 0, Node (Leaf, 2, Leaf)) );
              ( (Node (Node (Leaf, 2, Leaf), 2, Node (Leaf, 1, Leaf)), 3),
                Node
                  (Node (Leaf, 2, Leaf), 2, Node (Leaf, 1, Node (Leaf, 3, Leaf)))
              );
              ((Leaf, 1), Node (Leaf, 1, Leaf));
              ( (Node (Node (Leaf, 2, Leaf), 3, Node (Leaf, 1, Leaf)), 1),
                Node
                  (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Node (Leaf, 1, Leaf))
              );
              ( (Node (Node (Leaf, 3, Node (Leaf, 1, Leaf)), 1, Leaf), 1),
                Node (Node (Leaf, 3, Node (Leaf, 1, Leaf)), 1, Leaf) );
              ( ( Node
                    ( Leaf,
                      2,
                      Node (Node (Leaf, 0, Node (Leaf, 1, Leaf)), 0, Leaf) ),
                  0 ),
                Node
                  ( Node (Leaf, 0, Leaf),
                    2,
                    Node (Node (Leaf, 0, Node (Leaf, 1, Leaf)), 0, Leaf) ) );
              ( (Node (Leaf, 3, Node (Leaf, 3, Node (Leaf, 1, Leaf))), 2),
                Node
                  (Node (Leaf, 2, Leaf), 3, Node (Leaf, 3, Node (Leaf, 1, Leaf)))
              );
              ( ( Node
                    ( Node (Node (Leaf, 2, Leaf), 3, Node (Leaf, 1, Leaf)),
                      3,
                      Leaf ),
                  1 ),
                Node
                  ( Node
                      ( Node (Node (Leaf, 1, Leaf), 2, Leaf),
                        3,
                        Node (Leaf, 1, Leaf) ),
                    3,
                    Leaf ) );
              ( ( Node
                    ( Node (Node (Node (Leaf, 2, Leaf), 3, Leaf), 1, Leaf),
                      2,
                      Leaf ),
                  0 ),
                Node
                  ( Node
                      ( Node (Node (Node (Leaf, 0, Leaf), 2, Leaf), 3, Leaf),
                        1,
                        Leaf ),
                    2,
                    Leaf ) );
              ( ( Node
                    ( Leaf,
                      3,
                      Node (Node (Leaf, 2, Leaf), 3, Node (Leaf, 0, Leaf)) ),
                  1 ),
                Node
                  ( Node (Leaf, 1, Leaf),
                    3,
                    Node (Node (Leaf, 2, Leaf), 3, Node (Leaf, 0, Leaf)) ) );
              ( ( Node
                    ( Node (Leaf, 2, Node (Leaf, 0, Leaf)),
                      3,
                      Node (Leaf, 0, Leaf) ),
                  2 ),
                Node
                  (Node (Leaf, 2, Node (Leaf, 0, Leaf)), 3, Node (Leaf, 0, Leaf))
              );
              ( ( Node
                    ( Node (Leaf, 0, Leaf),
                      0,
                      Node (Node (Leaf, 3, Leaf), 0, Leaf) ),
                  0 ),
                Node
                  (Node (Leaf, 0, Leaf), 0, Node (Node (Leaf, 3, Leaf), 0, Leaf))
              );
              ( (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 0, Leaf)), 0),
                Node
                  (Node (Node (Leaf, 0, Leaf), 1, Leaf), 2, Node (Leaf, 0, Leaf))
              );
              ( (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 1, Leaf)), 1),
                Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 1, Leaf)) );
              ( ( Node
                    ( Node (Leaf, 0, Leaf),
                      1,
                      Node (Node (Leaf, 1, Leaf), 1, Leaf) ),
                  0 ),
                Node
                  (Node (Leaf, 0, Leaf), 1, Node (Node (Leaf, 1, Leaf), 1, Leaf))
              );
            ];
          background = [ "less_than"; "equal_to" ];
        } );
    ( "tree_inorder_bool",
      proj
        {
          function_name = "tree_inorder_bool";
          k_max = 20;
          d_in = Denotation.tree Denotation.bool;
          d_out = Denotation.list Denotation.bool;
          input = Sample2.bool_tree;
          func =
            (let f : bool Tree2.t -> bool list =
              fun tree -> Tree2.in_order tree
             in
             f);
          assertion =
            [
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Leaf,
                            true,
                            Node
                              ( Node (Leaf, false, Node (Leaf, true, Leaf)),
                                true,
                                Leaf ) ),
                        false,
                        Leaf ) ),
                [ false; true; false; true; true; false ] );
              (Leaf, []);
              ( Node (Leaf, true, Node (Node (Leaf, true, Leaf), false, Leaf)),
                [ true; true; false ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Leaf,
                        true,
                        Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, true, Leaf)) ) ) ),
                [ false; true; true; true; true ] );
              ( Node
                  ( Node
                      ( Node
                          ( Node (Node (Leaf, false, Leaf), true, Leaf),
                            true,
                            Node (Leaf, false, Leaf) ),
                        false,
                        Leaf ),
                    true,
                    Leaf ),
                [ false; true; true; false; false; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Node
                              ( Leaf,
                                true,
                                Node (Node (Leaf, true, Leaf), true, Leaf) ),
                            false,
                            Leaf ),
                        true,
                        Leaf ) ),
                [ true; true; true; true; false; true ] );
              ( Node
                  ( Node
                      (Node (Node (Leaf, true, Leaf), false, Leaf), true, Leaf),
                    true,
                    Leaf ),
                [ true; false; true; true ] );
              ( Node
                  ( Node (Leaf, true, Node (Leaf, false, Leaf)),
                    true,
                    Node (Node (Node (Leaf, true, Leaf), true, Leaf), true, Leaf)
                  ),
                [ true; false; true; true; true; true ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node (Node (Leaf, true, Leaf), true, Leaf) ),
                        false,
                        Leaf ) ),
                [ false; false; true; true; false ] );
              ( Node
                  ( Node
                      (Leaf, false, Node (Node (Leaf, true, Leaf), true, Leaf)),
                    true,
                    Node (Leaf, true, Leaf) ),
                [ false; true; true; true; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node
                              ( Node (Leaf, false, Leaf),
                                true,
                                Node (Leaf, false, Leaf) ) ),
                        true,
                        Leaf ) ),
                [ true; false; false; true; false; true ] );
              ( Node
                  ( Node (Node (Leaf, false, Leaf), true, Leaf),
                    true,
                    Node (Node (Leaf, false, Leaf), true, Leaf) ),
                [ false; true; true; false; true ] );
              ( Node
                  ( Node (Node (Leaf, true, Leaf), true, Leaf),
                    false,
                    Node
                      (Node (Leaf, true, Leaf), false, Node (Leaf, true, Leaf))
                  ),
                [ true; true; false; true; false; true ] );
              ( Node
                  ( Node (Leaf, false, Leaf),
                    true,
                    Node
                      ( Node
                          ( Node (Leaf, true, Node (Leaf, false, Leaf)),
                            false,
                            Leaf ),
                        true,
                        Leaf ) ),
                [ false; true; true; false; false; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, true, Leaf)) ),
                        true,
                        Node (Leaf, false, Leaf) ) ),
                [ true; true; true; true; true; false ] );
              ( Node
                  ( Node
                      ( Node
                          ( Node
                              ( Node (Leaf, true, Leaf),
                                false,
                                Node (Leaf, false, Leaf) ),
                            false,
                            Leaf ),
                        true,
                        Leaf ),
                    true,
                    Leaf ),
                [ true; false; false; false; true; true ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      (Leaf, false, Node (Leaf, true, Node (Leaf, true, Leaf)))
                  ),
                [ false; false; true; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Leaf,
                        false,
                        Node
                          ( Node (Node (Leaf, false, Leaf), true, Leaf),
                            false,
                            Leaf ) ) ),
                [ true; false; false; true; false ] );
              ( Node
                  ( Node
                      ( Leaf,
                        true,
                        Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, false, Leaf)) ) ),
                    false,
                    Leaf ),
                [ true; true; true; false; false ] );
              ( Node
                  ( Node (Leaf, false, Leaf),
                    true,
                    Node (Node (Leaf, false, Leaf), false, Leaf) ),
                [ false; true; false; false ] );
            ];
          background = [ "append_bool" ];
        } );
    ( "tree_count_leaves",
      proj
        {
          function_name = "tree_count_leaves";
          k_max = 15;
          d_in = Denotation.tree Denotation.bool;
          d_out = Denotation.int;
          input = Sample2.bool_tree;
          func =
            (let f : bool Tree2.t -> int =
              fun tree -> Tree2.count_leaves tree
             in
             f);
          assertion =
            [
              ( Node (Node (Leaf, false, Node (Leaf, true, Leaf)), false, Leaf),
                4 );
              (Leaf, 1);
              ( Node
                  ( Node
                      ( Node
                          ( Leaf,
                            true,
                            Node (Leaf, false, Node (Leaf, true, Leaf)) ),
                        false,
                        Node (Leaf, false, Leaf) ),
                    false,
                    Leaf ),
                7 );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Node
                              ( Node (Leaf, true, Leaf),
                                true,
                                Node (Leaf, true, Leaf) ),
                            false,
                            Leaf ),
                        false,
                        Leaf ) ),
                7 );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Leaf,
                        true,
                        Node
                          ( Node (Node (Leaf, false, Leaf), false, Leaf),
                            true,
                            Node (Leaf, true, Leaf) ) ) ),
                7 );
              ( Node
                  ( Node
                      ( Node
                          ( Node (Node (Leaf, true, Leaf), true, Leaf),
                            true,
                            Node (Leaf, false, Leaf) ),
                        false,
                        Leaf ),
                    false,
                    Leaf ),
                7 );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node
                              ( Node (Leaf, true, Node (Leaf, true, Leaf)),
                                false,
                                Leaf ) ),
                        false,
                        Leaf ) ),
                7 );
              ( Node
                  ( Node
                      ( Leaf,
                        false,
                        Node
                          ( Leaf,
                            true,
                            Node (Leaf, false, Node (Leaf, false, Leaf)) ) ),
                    true,
                    Node (Leaf, true, Leaf) ),
                7 );
              ( Node
                  ( Node (Leaf, true, Leaf),
                    false,
                    Node
                      ( Node
                          ( Node (Node (Leaf, true, Leaf), true, Leaf),
                            true,
                            Leaf ),
                        false,
                        Leaf ) ),
                7 );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node
                              ( Node (Node (Leaf, true, Leaf), false, Leaf),
                                true,
                                Leaf ) ),
                        false,
                        Leaf ) ),
                7 );
              ( Node
                  ( Node
                      ( Node
                          ( Node (Leaf, true, Node (Leaf, true, Leaf)),
                            false,
                            Leaf ),
                        false,
                        Leaf ),
                    true,
                    Node (Leaf, false, Leaf) ),
                7 );
              ( Node
                  ( Node
                      ( Leaf,
                        true,
                        Node
                          ( Node (Node (Leaf, true, Leaf), true, Leaf),
                            false,
                            Node (Leaf, true, Leaf) ) ),
                    true,
                    Leaf ),
                7 );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Node (Leaf, false, Node (Leaf, false, Leaf)),
                            true,
                            Leaf ),
                        true,
                        Node (Leaf, true, Leaf) ) ),
                7 );
              ( Node
                  ( Node (Leaf, false, Leaf),
                    true,
                    Node
                      ( Node
                          ( Node (Node (Leaf, true, Leaf), false, Leaf),
                            true,
                            Leaf ),
                        true,
                        Leaf ) ),
                7 );
              ( Node
                  ( Node
                      ( Leaf,
                        false,
                        Node
                          ( Node (Leaf, true, Node (Leaf, true, Leaf)),
                            true,
                            Leaf ) ),
                    false,
                    Node (Leaf, true, Leaf) ),
                7 );
            ];
          background = [ "add" ];
        } );
    ( "tree_count_nodes",
      proj
        {
          function_name = "tree_count_nodes";
          k_max = 15;
          d_in = Denotation.tree Denotation.int;
          d_out = Denotation.int;
          input = Sample2.nat_tree;
          func =
            (let f : int Tree2.t -> int = fun tree -> Tree2.count_nodes tree in
             f);
          assertion =
            [
              ( Node
                  (Node (Leaf, 1, Leaf), 0, Node (Leaf, 2, Node (Leaf, 3, Leaf))),
                4 );
              (Leaf, 0);
              ( Node
                  (Leaf, 3, Node (Leaf, 2, Node (Leaf, 1, Node (Leaf, 0, Leaf)))),
                4 );
              ( Node
                  (Leaf, 1, Node (Node (Node (Leaf, 1, Leaf), 1, Leaf), 3, Leaf)),
                4 );
              ( Node
                  (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 2, Node (Leaf, 2, Leaf)),
                4 );
              (Node (Node (Leaf, 0, Leaf), 2, Leaf), 2);
              ( Node
                  (Node (Leaf, 2, Node (Node (Leaf, 1, Leaf), 2, Leaf)), 0, Leaf),
                4 );
              (Node (Leaf, 2, Node (Leaf, 0, Leaf)), 2);
              ( Node
                  (Node (Leaf, 1, Node (Leaf, 0, Node (Leaf, 1, Leaf))), 0, Leaf),
                4 );
              (Node (Node (Leaf, 1, Leaf), 3, Node (Leaf, 0, Leaf)), 3);
              ( Node
                  (Leaf, 1, Node (Leaf, 1, Node (Leaf, 3, Node (Leaf, 1, Leaf)))),
                4 );
              ( Node
                  (Node (Node (Leaf, 3, Leaf), 3, Leaf), 1, Node (Leaf, 0, Leaf)),
                4 );
              (Node (Node (Leaf, 0, Leaf), 3, Node (Leaf, 3, Leaf)), 3);
              ( Node
                  (Leaf, 1, Node (Node (Leaf, 3, Leaf), 1, Node (Leaf, 0, Leaf))),
                4 );
              ( Node
                  (Leaf, 3, Node (Node (Leaf, 2, Node (Leaf, 2, Leaf)), 0, Leaf)),
                4 );
            ];
          background = [ "add" ];
        } );
    ( "tree_inorder",
      proj
        {
          function_name = "tree_inorder";
          k_max = 15;
          d_in = Denotation.tree Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_tree;
          func =
            (let f : int Tree2.t -> int list =
              fun tree -> Tree2.in_order tree
             in
             f);
          assertion =
            [
              ( Node
                  (Node (Node (Leaf, 3, Node (Leaf, 2, Leaf)), 1, Leaf), 2, Leaf),
                [ 3; 2; 1; 2 ] );
              (Leaf, []);
              ( Node
                  (Node (Leaf, 1, Node (Leaf, 0, Node (Leaf, 1, Leaf))), 1, Leaf),
                [ 1; 0; 1; 1 ] );
              ( Node
                  (Leaf, 3, Node (Node (Leaf, 3, Leaf), 3, Node (Leaf, 0, Leaf))),
                [ 3; 3; 3; 0 ] );
              (Node (Leaf, 0, Leaf), [ 0 ]);
              ( Node
                  (Leaf, 3, Node (Node (Leaf, 1, Node (Leaf, 2, Leaf)), 1, Leaf)),
                [ 3; 1; 2; 1 ] );
              ( Node
                  (Node (Node (Leaf, 1, Node (Leaf, 2, Leaf)), 0, Leaf), 2, Leaf),
                [ 1; 2; 0; 2 ] );
              ( Node
                  (Node (Leaf, 1, Node (Node (Leaf, 0, Leaf), 0, Leaf)), 1, Leaf),
                [ 1; 0; 0; 1 ] );
              ( Node
                  (Leaf, 1, Node (Leaf, 1, Node (Node (Leaf, 2, Leaf), 0, Leaf))),
                [ 1; 1; 2; 0 ] );
              ( Node
                  (Leaf, 2, Node (Node (Node (Leaf, 1, Leaf), 3, Leaf), 2, Leaf)),
                [ 2; 1; 3; 2 ] );
              ( Node
                  (Node (Node (Leaf, 1, Node (Leaf, 2, Leaf)), 1, Leaf), 0, Leaf),
                [ 1; 2; 1; 0 ] );
              ( Node
                  (Leaf, 3, Node (Node (Node (Leaf, 2, Leaf), 1, Leaf), 1, Leaf)),
                [ 3; 2; 1; 1 ] );
              (Node (Node (Leaf, 1, Leaf), 0, Node (Leaf, 0, Leaf)), [ 1; 0; 0 ]);
              (Node (Node (Leaf, 2, Node (Leaf, 1, Leaf)), 3, Leaf), [ 2; 1; 3 ]);
              ( Node
                  (Leaf, 2, Node (Node (Leaf, 3, Node (Leaf, 0, Leaf)), 1, Leaf)),
                [ 2; 3; 0; 1 ] );
            ];
          background = [ "append" ];
        } );
    ( "tree_map",
      proj
        {
          function_name = "tree_map";
          k_max = 20;
          d_in =
            Denotation.args2 Denotation.var (Denotation.tree Denotation.int);
          d_out = Denotation.tree Denotation.int;
          input =
            Sample2.pair (Sample2.from ("div2", [ "inc" ])) Sample2.nat_tree;
          func =
            (let f : string * int Tree2.t -> int Tree2.t =
              fun (fname, t) ->
               let mapper =
                 match fname with
                 | "div2" -> fun x -> x / 2
                 | "inc" -> fun x -> x + 1
                 | _ -> failwith ("Unknown Myth built-in '" ^ fname ^ "'")
               in
               Tree2.map mapper t
             in
             f);
          assertion =
            [
              ( ( "div2",
                  Node
                    ( Node (Leaf, 3, Node (Node (Leaf, 1, Leaf), 2, Leaf)),
                      0,
                      Leaf ) ),
                Node
                  (Node (Leaf, 1, Node (Node (Leaf, 0, Leaf), 1, Leaf)), 0, Leaf)
              );
              (("div2", Leaf), Leaf);
              ( ("div2", Node (Node (Leaf, 1, Node (Leaf, 0, Leaf)), 1, Leaf)),
                Node (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 0, Leaf) );
              ( ( "div2",
                  Node
                    ( Node (Node (Leaf, 0, Node (Leaf, 3, Leaf)), 3, Leaf),
                      1,
                      Leaf ) ),
                Node
                  (Node (Node (Leaf, 0, Node (Leaf, 1, Leaf)), 1, Leaf), 0, Leaf)
              );
              ( ("div2", Node (Leaf, 3, Node (Node (Leaf, 1, Leaf), 1, Leaf))),
                Node (Leaf, 1, Node (Node (Leaf, 0, Leaf), 0, Leaf)) );
              ( ( "div2",
                  Node
                    ( Node (Node (Leaf, 0, Leaf), 0, Leaf),
                      1,
                      Node (Leaf, 0, Leaf) ) ),
                Node
                  (Node (Node (Leaf, 0, Leaf), 0, Leaf), 0, Node (Leaf, 0, Leaf))
              );
              ( ( "div2",
                  Node
                    ( Node (Node (Node (Leaf, 2, Leaf), 3, Leaf), 1, Leaf),
                      1,
                      Leaf ) ),
                Node
                  (Node (Node (Node (Leaf, 1, Leaf), 1, Leaf), 0, Leaf), 0, Leaf)
              );
              (("div2", Node (Leaf, 0, Leaf)), Node (Leaf, 0, Leaf));
              ( ( "inc",
                  Node
                    ( Node (Leaf, 1, Node (Node (Leaf, 2, Leaf), 3, Leaf)),
                      0,
                      Leaf ) ),
                Node
                  (Node (Leaf, 2, Node (Node (Leaf, 3, Leaf), 4, Leaf)), 1, Leaf)
              );
              ( ( "div2",
                  Node
                    ( Node (Leaf, 3, Leaf),
                      2,
                      Node (Node (Leaf, 2, Leaf), 1, Leaf) ) ),
                Node
                  (Node (Leaf, 1, Leaf), 1, Node (Node (Leaf, 1, Leaf), 0, Leaf))
              );
              ( ( "div2",
                  Node
                    ( Leaf,
                      1,
                      Node (Leaf, 2, Node (Node (Leaf, 1, Leaf), 2, Leaf)) ) ),
                Node
                  (Leaf, 0, Node (Leaf, 1, Node (Node (Leaf, 0, Leaf), 1, Leaf)))
              );
              (("inc", Node (Leaf, 1, Leaf)), Node (Leaf, 2, Leaf));
              ( ( "div2",
                  Node
                    ( Leaf,
                      1,
                      Node (Leaf, 0, Node (Node (Leaf, 2, Leaf), 0, Leaf)) ) ),
                Node
                  (Leaf, 0, Node (Leaf, 0, Node (Node (Leaf, 1, Leaf), 0, Leaf)))
              );
              ( ( "div2",
                  Node
                    ( Node (Leaf, 0, Leaf),
                      1,
                      Node (Leaf, 0, Node (Leaf, 0, Leaf)) ) ),
                Node
                  (Node (Leaf, 0, Leaf), 0, Node (Leaf, 0, Node (Leaf, 0, Leaf)))
              );
              ( ("div2", Node (Node (Node (Leaf, 1, Leaf), 0, Leaf), 2, Leaf)),
                Node (Node (Node (Leaf, 0, Leaf), 0, Leaf), 1, Leaf) );
              ( ( "div2",
                  Node
                    ( Leaf,
                      3,
                      Node (Node (Leaf, 1, Node (Leaf, 0, Leaf)), 0, Leaf) ) ),
                Node
                  (Leaf, 1, Node (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 0, Leaf))
              );
              ( ( "inc",
                  Node
                    ( Node (Leaf, 3, Leaf),
                      0,
                      Node (Node (Leaf, 0, Leaf), 2, Leaf) ) ),
                Node
                  (Node (Leaf, 4, Leaf), 1, Node (Node (Leaf, 1, Leaf), 3, Leaf))
              );
              ( ("inc", Node (Node (Leaf, 1, Node (Leaf, 0, Leaf)), 2, Leaf)),
                Node (Node (Leaf, 2, Node (Leaf, 1, Leaf)), 3, Leaf) );
              ( ( "inc",
                  Node
                    ( Leaf,
                      0,
                      Node (Leaf, 1, Node (Node (Leaf, 1, Leaf), 1, Leaf)) ) ),
                Node
                  (Leaf, 1, Node (Leaf, 2, Node (Node (Leaf, 2, Leaf), 2, Leaf)))
              );
              ( ( "div2",
                  Node
                    ( Leaf,
                      3,
                      Node (Node (Node (Leaf, 3, Leaf), 2, Leaf), 3, Leaf) ) ),
                Node
                  (Leaf, 1, Node (Node (Node (Leaf, 1, Leaf), 1, Leaf), 1, Leaf))
              );
            ];
          background = [ "div2"; "inc" ];
        } );
    ( "tree_nodes_at_level",
      proj
        {
          function_name = "tree_nodes_at_level";
          k_max = 20;
          d_in =
            Denotation.args2 (Denotation.tree Denotation.bool) Denotation.int;
          d_out = Denotation.int;
          input = Sample2.pair Sample2.bool_tree Sample2.nat;
          func =
            (let f : bool Tree2.t * int -> int =
              fun (tree, level) -> Tree2.count_nodes_at_level level tree
             in
             f);
          assertion =
            [
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        (Leaf, false, Node (Node (Leaf, true, Leaf), true, Leaf))
                    ),
                  1 ),
                1 );
              ((Leaf, 0), 0);
              ( ( Node
                    ( Node
                        ( Leaf,
                          false,
                          Node
                            ( Node
                                ( Leaf,
                                  true,
                                  Node (Node (Leaf, true, Leaf), true, Leaf) ),
                              false,
                              Leaf ) ),
                      false,
                      Leaf ),
                  1 ),
                1 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node (Leaf, false, Leaf),
                          true,
                          Node (Leaf, false, Leaf) ) ),
                  0 ),
                1 );
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        ( Node
                            ( Node (Leaf, false, Node (Leaf, false, Leaf)),
                              false,
                              Node (Leaf, false, Leaf) ),
                          true,
                          Leaf ) ),
                  3 ),
                2 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node
                            ( Node (Leaf, false, Leaf),
                              false,
                              Node (Leaf, true, Leaf) ),
                          true,
                          Node (Leaf, false, Leaf) ) ),
                  1 ),
                1 );
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        ( Node
                            ( Node (Leaf, false, Leaf),
                              true,
                              Node (Leaf, true, Leaf) ),
                          true,
                          Node (Leaf, false, Leaf) ) ),
                  2 ),
                2 );
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        ( Node
                            ( Node
                                ( Leaf,
                                  false,
                                  Node (Node (Leaf, false, Leaf), false, Leaf)
                                ),
                              false,
                              Leaf ),
                          false,
                          Leaf ) ),
                  2 ),
                1 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node
                            ( Node
                                ( Node (Node (Leaf, false, Leaf), false, Leaf),
                                  true,
                                  Leaf ),
                              true,
                              Leaf ),
                          true,
                          Leaf ) ),
                  2 ),
                1 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node
                            ( Leaf,
                              false,
                              Node
                                ( Node (Leaf, false, Leaf),
                                  false,
                                  Node (Leaf, false, Leaf) ) ),
                          true,
                          Leaf ) ),
                  1 ),
                1 );
              ( ( Node
                    ( Node
                        ( Leaf,
                          false,
                          Node
                            ( Node
                                ( Leaf,
                                  false,
                                  Node (Node (Leaf, true, Leaf), false, Leaf) ),
                              true,
                              Leaf ) ),
                      true,
                      Leaf ),
                  1 ),
                1 );
              ( ( Node
                    ( Node
                        ( Node (Node (Leaf, false, Leaf), false, Leaf),
                          false,
                          Leaf ),
                      false,
                      Node (Node (Leaf, false, Leaf), false, Leaf) ),
                  0 ),
                1 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node
                            ( Leaf,
                              true,
                              Node (Leaf, false, Node (Leaf, false, Leaf)) ),
                          false,
                          Leaf ) ),
                  2 ),
                1 );
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        ( Leaf,
                          false,
                          Node
                            ( Leaf,
                              true,
                              Node
                                ( Node (Node (Leaf, true, Leaf), false, Leaf),
                                  true,
                                  Leaf ) ) ) ),
                  0 ),
                1 );
              ( ( Node
                    ( Node
                        (Node (Node (Leaf, true, Leaf), false, Leaf), true, Leaf),
                      true,
                      Leaf ),
                  0 ),
                1 );
              ( ( Node
                    ( Node
                        ( Leaf,
                          true,
                          Node
                            ( Leaf,
                              true,
                              Node (Leaf, false, Node (Leaf, true, Leaf)) ) ),
                      true,
                      Node (Leaf, false, Leaf) ),
                  2 ),
                1 );
              ( ( Node
                    ( Node
                        ( Leaf,
                          false,
                          Node
                            ( Node (Leaf, false, Node (Leaf, false, Leaf)),
                              false,
                              Node (Leaf, true, Leaf) ) ),
                      true,
                      Leaf ),
                  1 ),
                1 );
              ( ( Node
                    ( Leaf,
                      false,
                      Node
                        ( Node
                            ( Node (Leaf, false, Leaf),
                              false,
                              Node (Leaf, true, Node (Leaf, true, Leaf)) ),
                          false,
                          Leaf ) ),
                  0 ),
                1 );
              ( ( Node
                    ( Leaf,
                      true,
                      Node
                        ( Node
                            ( Node (Leaf, true, Leaf),
                              true,
                              Node (Leaf, false, Leaf) ),
                          true,
                          Leaf ) ),
                  1 ),
                1 );
              ( ( Node
                    ( Node (Leaf, true, Node (Leaf, true, Leaf)),
                      false,
                      Node
                        (Node (Leaf, true, Node (Leaf, true, Leaf)), true, Leaf)
                    ),
                  2 ),
                2 );
            ];
          background = [ "add" ];
        } );
    ( "tree_postorder",
      proj
        {
          function_name = "tree_postorder";
          k_max = 20;
          d_in = Denotation.tree Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_tree;
          func =
            (let f : int Tree2.t -> int list =
              fun tree -> Tree2.post_order tree
             in
             f);
          assertion =
            [
              ( Node
                  (Leaf, 2, Node (Node (Node (Leaf, 3, Leaf), 2, Leaf), 1, Leaf)),
                [ 3; 2; 1; 2 ] );
              (Leaf, []);
              (Node (Leaf, 0, Leaf), [ 0 ]);
              ( Node
                  (Node (Node (Leaf, 2, Leaf), 1, Leaf), 1, Node (Leaf, 2, Leaf)),
                [ 2; 1; 2; 1 ] );
              (Node (Node (Leaf, 2, Leaf), 2, Leaf), [ 2; 2 ]);
              ( Node
                  (Node (Leaf, 1, Node (Leaf, 1, Leaf)), 0, Node (Leaf, 0, Leaf)),
                [ 1; 1; 0; 0 ] );
              (Node (Node (Node (Leaf, 0, Leaf), 1, Leaf), 3, Leaf), [ 0; 1; 3 ]);
              ( Node
                  (Node (Node (Leaf, 3, Leaf), 0, Leaf), 2, Node (Leaf, 0, Leaf)),
                [ 3; 0; 0; 2 ] );
              ( Node
                  (Node (Leaf, 1, Node (Leaf, 2, Node (Leaf, 1, Leaf))), 1, Leaf),
                [ 1; 2; 1; 1 ] );
              (Node (Node (Leaf, 1, Leaf), 3, Node (Leaf, 0, Leaf)), [ 1; 0; 3 ]);
              (Node (Leaf, 0, Node (Node (Leaf, 2, Leaf), 2, Leaf)), [ 2; 2; 0 ]);
              ( Node
                  (Node (Leaf, 2, Node (Node (Leaf, 1, Leaf), 0, Leaf)), 1, Leaf),
                [ 1; 0; 2; 1 ] );
              ( Node
                  (Node (Node (Leaf, 0, Leaf), 3, Leaf), 1, Node (Leaf, 1, Leaf)),
                [ 0; 3; 1; 1 ] );
              ( Node
                  (Leaf, 0, Node (Node (Leaf, 1, Leaf), 3, Node (Leaf, 3, Leaf))),
                [ 1; 3; 3; 0 ] );
              (Node (Node (Leaf, 2, Node (Leaf, 2, Leaf)), 1, Leaf), [ 2; 2; 1 ]);
              ( Node
                  (Leaf, 2, Node (Node (Leaf, 0, Node (Leaf, 0, Leaf)), 1, Leaf)),
                [ 0; 0; 1; 2 ] );
              (Node (Node (Leaf, 3, Leaf), 3, Node (Leaf, 0, Leaf)), [ 3; 0; 3 ]);
              ( Node
                  (Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 0, Leaf)), 3, Leaf),
                [ 2; 0; 0; 3 ] );
              (Node (Leaf, 3, Node (Node (Leaf, 2, Leaf), 0, Leaf)), [ 2; 0; 3 ]);
              ( Node
                  (Node (Leaf, 3, Node (Node (Leaf, 1, Leaf), 0, Leaf)), 2, Leaf),
                [ 1; 0; 3; 2 ] );
            ];
          background = [ "append" ];
        } );
    ( "tree_preorder",
      proj
        {
          function_name = "tree_preorder";
          k_max = 15;
          d_in = Denotation.tree Denotation.int;
          d_out = Denotation.list Denotation.int;
          input = Sample2.nat_tree;
          func =
            (let f : int Tree2.t -> int list =
              fun tree -> Tree2.pre_order tree
             in
             f);
          assertion =
            [
              ( Node
                  (Leaf, 1, Node (Node (Leaf, 2, Leaf), 1, Node (Leaf, 0, Leaf))),
                [ 1; 1; 2; 0 ] );
              (Leaf, []);
              (Node (Leaf, 2, Node (Node (Leaf, 1, Leaf), 2, Leaf)), [ 2; 2; 1 ]);
              (Node (Leaf, 2, Node (Leaf, 1, Node (Leaf, 3, Leaf))), [ 2; 1; 3 ]);
              (Node (Node (Node (Leaf, 1, Leaf), 3, Leaf), 3, Leaf), [ 3; 3; 1 ]);
              ( Node
                  (Node (Node (Leaf, 2, Node (Leaf, 1, Leaf)), 3, Leaf), 2, Leaf),
                [ 2; 3; 2; 1 ] );
              (Node (Node (Leaf, 3, Leaf), 0, Leaf), [ 0; 3 ]);
              (Node (Leaf, 1, Node (Node (Leaf, 2, Leaf), 0, Leaf)), [ 1; 0; 2 ]);
              ( Node
                  (Node (Leaf, 2, Leaf), 3, Node (Leaf, 0, Node (Leaf, 3, Leaf))),
                [ 3; 2; 0; 3 ] );
              ( Node
                  (Node (Leaf, 3, Leaf), 3, Node (Leaf, 1, Node (Leaf, 1, Leaf))),
                [ 3; 3; 1; 1 ] );
              (Node (Node (Leaf, 2, Node (Leaf, 3, Leaf)), 2, Leaf), [ 2; 2; 3 ]);
              ( Node
                  (Node (Node (Leaf, 0, Leaf), 1, Leaf), 1, Node (Leaf, 0, Leaf)),
                [ 1; 1; 0; 0 ] );
              (Node (Leaf, 1, Node (Node (Leaf, 2, Leaf), 2, Leaf)), [ 1; 2; 2 ]);
              ( Node
                  (Node (Node (Leaf, 2, Leaf), 1, Node (Leaf, 0, Leaf)), 0, Leaf),
                [ 0; 1; 2; 0 ] );
              ( Node
                  (Leaf, 0, Node (Node (Leaf, 1, Node (Leaf, 2, Leaf)), 0, Leaf)),
                [ 0; 0; 1; 2 ] );
            ];
          background = [ "append" ];
        } );
    ( "list_last2",
    proj
      {
        function_name = "list_last2";
          k_max = 20;
          d_in = Denotation.list Denotation.int;
          d_out = Denotation.opt (Denotation.args2 Denotation.int Denotation.int);
          input = Sample2.nat_list;
          assertion =
          [
            ([], None);
            ([1], None);
            ([2;1], Some(2,1));
            ([1;2], Some(1,2));
            ([3;2;1], Some(2,1));
            ([4;1;3], Some(1,3));
          ];
        func = 
         (let rec f: int list -> (int * int) option =
           fun xs ->
            match xs with
            | [] -> None
            | hd::tl -> match tl with
                        | [] -> None
                        | hd2::tl2 -> match tl2 with
                                      | [] -> Some(hd, hd2)
                                      | _ -> f tl
          in
         f);
         background = [];
      });
    ( "list_make",
    proj
     {
      function_name = "list_make";
        k_max = 20;
        d_in = Denotation.int;
        d_out = Denotation.list Denotation.int;
        input = Sample2.nat;
        assertion =
        [
          (0, []);
          (3, [0;0;0]);
          (2, [0;0]);
        ];
        func =
        (
          let rec f: int -> int list =
            fun n ->
             if n = 0 then []
             else [n] @ f (n-1)
        in
        f);
        background = [];
     });
     ( "bool_always_true",
      proj
        {
          function_name = "bool_always_true";
          k_max = 2;
          d_in = Denotation.bool;
          d_out = Denotation.bool;
          assertion = [ (true, true); (false, true) ];
          input = Sample2.bool;
          func =
            (let f : bool -> bool = fun x -> true in
             f);
          background = [];
        } );
      ( "bool_always_false",
      proj
        {
          function_name = "bool_always_false";
          k_max = 2;
          d_in = Denotation.bool;
          d_out = Denotation.bool;
          assertion = [ (true, false); (false, false) ];
          input = Sample2.bool;
          func =
            (let f : bool -> bool = fun x -> false in
             f);
          background = [];
        } );
      ( "tree_collect_leaves",
      proj
        {
          function_name = "tree_collect_leaves";
          k_max = 20;
          d_in = Denotation.tree Denotation.bool;
          d_out = Denotation.list Denotation.bool;
          input = Sample2.bool_tree;
          func =
            (let f : bool Tree2.t -> bool list =
              fun tree -> Tree2.in_order tree
             in
             f);
          assertion =
            [
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Leaf,
                            true,
                            Node
                              ( Node (Leaf, false, Node (Leaf, true, Leaf)),
                                true,
                                Leaf ) ),
                        false,
                        Leaf ) ),
                [ false; true; false; true; true; false ] );
              (Leaf, []);
              ( Node (Leaf, true, Node (Node (Leaf, true, Leaf), false, Leaf)),
                [ true; true; false ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Leaf,
                        true,
                        Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, true, Leaf)) ) ) ),
                [ false; true; true; true; true ] );
              ( Node
                  ( Node
                      ( Node
                          ( Node (Node (Leaf, false, Leaf), true, Leaf),
                            true,
                            Node (Leaf, false, Leaf) ),
                        false,
                        Leaf ),
                    true,
                    Leaf ),
                [ false; true; true; false; false; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Node
                              ( Leaf,
                                true,
                                Node (Node (Leaf, true, Leaf), true, Leaf) ),
                            false,
                            Leaf ),
                        true,
                        Leaf ) ),
                [ true; true; true; true; false; true ] );
              ( Node
                  ( Node
                      (Node (Node (Leaf, true, Leaf), false, Leaf), true, Leaf),
                    true,
                    Leaf ),
                [ true; false; true; true ] );
              ( Node
                  ( Node (Leaf, true, Node (Leaf, false, Leaf)),
                    true,
                    Node (Node (Node (Leaf, true, Leaf), true, Leaf), true, Leaf)
                  ),
                [ true; false; true; true; true; true ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node (Node (Leaf, true, Leaf), true, Leaf) ),
                        false,
                        Leaf ) ),
                [ false; false; true; true; false ] );
              ( Node
                  ( Node
                      (Leaf, false, Node (Node (Leaf, true, Leaf), true, Leaf)),
                    true,
                    Node (Leaf, true, Leaf) ),
                [ false; true; true; true; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Leaf,
                            false,
                            Node
                              ( Node (Leaf, false, Leaf),
                                true,
                                Node (Leaf, false, Leaf) ) ),
                        true,
                        Leaf ) ),
                [ true; false; false; true; false; true ] );
              ( Node
                  ( Node (Node (Leaf, false, Leaf), true, Leaf),
                    true,
                    Node (Node (Leaf, false, Leaf), true, Leaf) ),
                [ false; true; true; false; true ] );
              ( Node
                  ( Node (Node (Leaf, true, Leaf), true, Leaf),
                    false,
                    Node
                      (Node (Leaf, true, Leaf), false, Node (Leaf, true, Leaf))
                  ),
                [ true; true; false; true; false; true ] );
              ( Node
                  ( Node (Leaf, false, Leaf),
                    true,
                    Node
                      ( Node
                          ( Node (Leaf, true, Node (Leaf, false, Leaf)),
                            false,
                            Leaf ),
                        true,
                        Leaf ) ),
                [ false; true; true; false; false; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, true, Leaf)) ),
                        true,
                        Node (Leaf, false, Leaf) ) ),
                [ true; true; true; true; true; false ] );
              ( Node
                  ( Node
                      ( Node
                          ( Node
                              ( Node (Leaf, true, Leaf),
                                false,
                                Node (Leaf, false, Leaf) ),
                            false,
                            Leaf ),
                        true,
                        Leaf ),
                    true,
                    Leaf ),
                [ true; false; false; false; true; true ] );
              ( Node
                  ( Leaf,
                    false,
                    Node
                      (Leaf, false, Node (Leaf, true, Node (Leaf, true, Leaf)))
                  ),
                [ false; false; true; true ] );
              ( Node
                  ( Leaf,
                    true,
                    Node
                      ( Leaf,
                        false,
                        Node
                          ( Node (Node (Leaf, false, Leaf), true, Leaf),
                            false,
                            Leaf ) ) ),
                [ true; false; false; true; false ] );
              ( Node
                  ( Node
                      ( Leaf,
                        true,
                        Node
                          ( Leaf,
                            true,
                            Node (Leaf, true, Node (Leaf, false, Leaf)) ) ),
                    false,
                    Leaf ),
                [ true; true; true; false; false ] );
              ( Node
                  ( Node (Leaf, false, Leaf),
                    true,
                    Node (Node (Leaf, false, Leaf), false, Leaf) ),
                [ false; true; false; false ] );
            ];
          background = [ "append_bool" ];
        } );
     (* ( "expr",
     proj
     {
        function_name = "expr";
        k_max = 20;
        d_in = Denotation.fomula;
        d_out = Denotation.int;

     }); *)
     (* ( "tree_height",
     proj
     {
        function_name = "tree_height";
        k_max = 20;
        d_in = Denotation.tree Denotation.int;
        d_out = Denotation.int;
        input = Sample2.nat_tree;

     }); *)
  ]
