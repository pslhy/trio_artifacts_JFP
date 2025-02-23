
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WITH
    | WILDCARD
    | VAL
    | UNIT
    | UID of (
# 37 "lib/parser.mly"
       (string)
# 18 "lib/parser.ml"
  )
    | TYPE
    | SYNTH
    | STR of (
# 38 "lib/parser.mly"
       (string)
# 25 "lib/parser.ml"
  )
    | STAR
    | SIG
    | SEMI
    | SATISFYING
    | RPAREN
    | RBRACKET
    | PIPE
    | OF
    | NEQ
    | MU
    | MATCH
    | LPAREN
    | LID of (
# 36 "lib/parser.mly"
       (string)
# 42 "lib/parser.ml"
  )
    | LET
    | LBRACKET
    | INT of (
# 41 "lib/parser.mly"
       (int)
# 49 "lib/parser.ml"
  )
    | INCLUDE
    | FUN
    | FORALL
    | FIX
    | FATEQ
    | EQUIV
    | EQ
    | EOF
    | END
    | DOT
    | COMMA
    | COLON
    | BINDING
    | ARR
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState59
  | MenhirState57
  | MenhirState52
  | MenhirState48
  | MenhirState44
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState34
  | MenhirState30
  | MenhirState23
  | MenhirState18
  | MenhirState14
  | MenhirState11
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "lib/parser.mly"
  
open Lang

let mk_ctor_by_name
      (c:string)
      (es:exp list)
    : exp =
  match c with
  | "Some" -> EOp(Just, es)
  | "None" -> EOp(No, [])
  | "T" -> EOp(True, [])
  | "F" -> EOp(False, [])
  | "Node" -> EOp (Node, match es with
			 | [l;v;r] -> [v;l;r]
			 | _ -> failwith "impossible")
  | "Leaf" -> EOp(Leaf, [])
  | _ -> failwith (c ^ " parser unimplemented")

let mk_type_by_name (c:string) : typ =
  match c with
  | "int" -> TNum
  | "bool" -> TBool
  | _ -> failwith (c ^ " parser unimplemented")

let mk_param_type_by_name (c:string) (t:typ): typ =
  match c with
  | "list" -> TList t
  | _ -> failwith (c ^ " parser unimplemented")

let rec rev_exps_to_exp ?(acc=EOp (Nil, [])) (es : exp list) : exp = 
  match es with
  | [] -> acc
  | hd::tl -> rev_exps_to_exp ~acc:(EOp (Cons, [hd; acc])) tl

# 133 "lib/parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_reduce3 : _menhir_env -> 'ttv_tail * _menhir_state * ((Lang.exp * Lang.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (exs : ((Lang.exp * Lang.exp) list))) = _menhir_stack in
    let _v : ((Lang.exp * Lang.exp) list) = 
# 90 "lib/parser.mly"
    ( exs )
# 143 "lib/parser.ml"
     in
    _menhir_goto_examples _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce11 : _menhir_env -> (((('ttv_tail * _menhir_state * (
# 37 "lib/parser.mly"
       (string)
# 155 "lib/parser.ml"
)) * _menhir_state) * _menhir_state * (Lang.exp))) * _menhir_state * (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, (c : (
# 37 "lib/parser.mly"
       (string)
# 161 "lib/parser.ml"
    ))), _), _, (e : (Lang.exp))), _, (es : (Lang.exp list))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 221 "lib/parser.mly"
                                  ( mk_ctor_by_name c (e :: List.rev es) )
# 166 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Lang.exp))) * _menhir_state * (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e : (Lang.exp))), _, (es : (Lang.exp list))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 223 "lib/parser.mly"
    ( ETuple (e :: List.rev es) )
# 176 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce15 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Lang.exp))) * _menhir_state * (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (e : (Lang.exp))), _, (es : (Lang.exp list))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 229 "lib/parser.mly"
    ( EOp (Cons, [e; rev_exps_to_exp es]) )
# 186 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_goto_nonempty_examples : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lang.exp * Lang.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_examples : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lang.exp * Lang.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : ((Lang.exp * Lang.exp) list)) = _v in
        Obj.magic _1
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (exs : ((Lang.exp * Lang.exp) list)) = _v in
        let (_menhir_stack, _menhir_s, (ex : (Lang.exp * Lang.exp))) = _menhir_stack in
        let _v : ((Lang.exp * Lang.exp) list) = 
# 95 "lib/parser.mly"
    ( ex::exs )
# 262 "lib/parser.ml"
         in
        _menhir_goto_nonempty_examples _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce9 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 37 "lib/parser.mly"
       (string)
# 271 "lib/parser.ml"
)) * _menhir_state * (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (c : (
# 37 "lib/parser.mly"
       (string)
# 277 "lib/parser.ml"
    ))), _, (e : (Lang.exp))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 217 "lib/parser.mly"
                     ( mk_ctor_by_name c [e] )
# 282 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 37 "lib/parser.mly"
       (string)
# 289 "lib/parser.ml"
)) * _menhir_state) * _menhir_state * (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (c : (
# 37 "lib/parser.mly"
       (string)
# 295 "lib/parser.ml"
    ))), _), _, (e : (Lang.exp))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 219 "lib/parser.mly"
                     ( mk_ctor_by_name c [e] )
# 300 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exp_comma_list_one : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce14 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
    let _v : (Lang.exp) = 
# 227 "lib/parser.mly"
    ( EOp (Cons, [e; EOp (Nil, [])]) )
# 416 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (es : (Lang.exp list))), _, (e : (Lang.exp))) = _menhir_stack in
        let _v : (Lang.exp list) = 
# 241 "lib/parser.mly"
    ( e :: es )
# 465 "lib/parser.ml"
         in
        _menhir_goto_exp_comma_list_one _menhir_env _menhir_stack _menhir_s _v
    | MenhirState59 | MenhirState52 | MenhirState44 | MenhirState23 | MenhirState18 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
        let _v : (Lang.exp list) = 
# 239 "lib/parser.mly"
    ( [e] )
# 475 "lib/parser.ml"
         in
        _menhir_goto_exp_comma_list_one _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState34 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (es : (Lang.exp))), _, (e : (Lang.exp))) = _menhir_stack in
        let _v : (Lang.exp * Lang.exp) = 
# 101 "lib/parser.mly"
    ( (es,e) )
# 582 "lib/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState34 in
                let _v : ((Lang.exp * Lang.exp) list) = 
# 91 "lib/parser.mly"
    ( [] )
# 610 "lib/parser.ml"
                 in
                _menhir_goto_examples _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (ex : (Lang.exp * Lang.exp))) = _menhir_stack in
            let _v : ((Lang.exp * Lang.exp) list) = 
# 97 "lib/parser.mly"
    ( [ex] )
# 623 "lib/parser.ml"
             in
            _menhir_goto_nonempty_examples _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LBRACKET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LID _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Lang.exp))) = _menhir_stack in
        Obj.magic _1

and _menhir_reduce8 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 37 "lib/parser.mly"
       (string)
# 740 "lib/parser.ml"
)) * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s, (c : (
# 37 "lib/parser.mly"
       (string)
# 746 "lib/parser.ml"
    ))), _) = _menhir_stack in
    let _v : (Lang.exp) = 
# 213 "lib/parser.mly"
    (
      mk_ctor_by_name c []
    )
# 753 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 36 "lib/parser.mly"
       (string)
# 760 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (x : (
# 36 "lib/parser.mly"
       (string)
# 765 "lib/parser.ml"
  )) ->
    let _v : (Lang.exp) = 
# 205 "lib/parser.mly"
    ( EVar x )
# 770 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : (Lang.exp) = 
# 225 "lib/parser.mly"
    ( EOp(Nil, []) )
# 780 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "lib/parser.mly"
       (int)
# 787 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (c : (
# 41 "lib/parser.mly"
       (int)
# 792 "lib/parser.ml"
  )) ->
    let _v : (Lang.exp) = 
# 207 "lib/parser.mly"
    ( Denotation.int c )
# 797 "lib/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "lib/parser.mly"
       (string)
# 804 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LID _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState39 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LBRACKET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState40 in
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | UID _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | UID _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "lib/parser.mly"
       (string)
# 930 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState1 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | LBRACKET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | UID _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | ARR | COMMA | EOF | RBRACKET | RPAREN | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (c : (
# 37 "lib/parser.mly"
       (string)
# 976 "lib/parser.ml"
        ))) = _menhir_stack in
        let _v : (Lang.exp) = 
# 209 "lib/parser.mly"
    (
      mk_ctor_by_name c []
    )
# 983 "lib/parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 36 "lib/parser.mly"
       (string)
# 1015 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState6 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "lib/parser.mly"
       (int)
# 1050 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 36 "lib/parser.mly"
       (string)
# 1059 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState48 in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 41 "lib/parser.mly"
       (int)
# 1092 "lib/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and examples : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Lang.exp * Lang.exp) list) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACKET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LID _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and exp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.exp) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LBRACKET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LID _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState38 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | LBRACKET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LID _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | UID _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | UID _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
