
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
# 14 "src/parser2.mly"
       (string)
# 18 "src/parser2.ml"
  )
    | TYPE
    | SYNTH
    | STR of (
# 15 "src/parser2.mly"
       (string)
# 25 "src/parser2.ml"
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
# 13 "src/parser2.mly"
       (string)
# 42 "src/parser2.ml"
  )
    | LET
    | LBRACKET
    | INT of (
# 18 "src/parser2.mly"
       (int)
# 49 "src/parser2.ml"
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
  | MenhirState49
  | MenhirState45
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState35
  | MenhirState31
  | MenhirState25
  | MenhirState22
  | MenhirState19
  | MenhirState15
  | MenhirState10
  | MenhirState5
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "src/parser2.mly"
  
open MyStdLib
open Lang

let mk_ctor_by_name
      (c:String.t)
      (e:Expr.t)
    : Expr.t =
  Expr.mk_ctor (Id.create c) e


# 107 "src/parser2.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_reduce4 : _menhir_env -> 'ttv_tail * _menhir_state * ((Lang.Expr.t list * Lang.Expr.t) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (exs : ((Lang.Expr.t list * Lang.Expr.t) list))) = _menhir_stack in
    let _v : ((Lang.Expr.t list * Lang.Expr.t) list) = 
# 67 "src/parser2.mly"
    ( exs )
# 117 "src/parser2.ml"
     in
    _menhir_goto_examples _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_examples : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lang.Expr.t list * Lang.Expr.t) list) -> 'ttv_return =
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
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_examples : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lang.Expr.t list * Lang.Expr.t) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : ((Lang.Expr.t list * Lang.Expr.t) list)) = _v in
        Obj.magic _1
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (exs : ((Lang.Expr.t list * Lang.Expr.t) list)) = _v in
        let (_menhir_stack, _menhir_s, (ex : (Lang.Expr.t list * Lang.Expr.t))) = _menhir_stack in
        let _v : ((Lang.Expr.t list * Lang.Expr.t) list) = 
# 72 "src/parser2.mly"
    ( ex::exs )
# 173 "src/parser2.ml"
         in
        _menhir_goto_nonempty_examples _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce12 : _menhir_env -> (((('ttv_tail * _menhir_state * (
# 14 "src/parser2.mly"
       (string)
# 187 "src/parser2.ml"
)) * _menhir_state) * _menhir_state * (Lang.Expr.t))) * _menhir_state * (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, (c : (
# 14 "src/parser2.mly"
       (string)
# 193 "src/parser2.ml"
    ))), _), _, (e : (Lang.Expr.t))), _, (es : (Lang.Expr.t list))) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 113 "src/parser2.mly"
  ( mk_ctor_by_name c (Expr.mk_tuple (e :: List.rev es)) )
# 198 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (es : (Lang.Expr.t list))) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 117 "src/parser2.mly"
    (List.fold
	  es
          ~f:( fun acc e ->
	      mk_ctor_by_name "Cons" (Expr.mk_tuple [e; acc])
          )
	~init:(mk_ctor_by_name "Nil" (Expr.mk_unit))
    )
# 214 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_goto_nonempty_exp_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                | LID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                | UID _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (es : (Lang.Expr.t list))) = _menhir_stack in
        let _v : (Lang.Expr.t list) = 
# 84 "src/parser2.mly"
    ( es )
# 286 "src/parser2.ml"
         in
        _menhir_goto_exp_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (es : (Lang.Expr.t list)) = _v in
    let (_menhir_stack, _menhir_s, (e : (Lang.Expr.t))) = _menhir_stack in
    let _v : (Lang.Expr.t list) = 
# 89 "src/parser2.mly"
    ( e::es )
# 301 "src/parser2.ml"
     in
    _menhir_goto_nonempty_exp_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_example : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.Expr.t list * Lang.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LID _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LPAREN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | UID _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState35 in
            let _v : ((Lang.Expr.t list * Lang.Expr.t) list) = 
# 68 "src/parser2.mly"
    ( [] )
# 333 "src/parser2.ml"
             in
            _menhir_goto_examples _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (ex : (Lang.Expr.t list * Lang.Expr.t))) = _menhir_stack in
        let _v : ((Lang.Expr.t list * Lang.Expr.t) list) = 
# 74 "src/parser2.mly"
    ( [ex] )
# 346 "src/parser2.ml"
         in
        _menhir_goto_nonempty_examples _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce11 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 14 "src/parser2.mly"
       (string)
# 359 "src/parser2.ml"
)) * _menhir_state * (Lang.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (c : (
# 14 "src/parser2.mly"
       (string)
# 365 "src/parser2.ml"
    ))), _, (e : (Lang.Expr.t))) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 111 "src/parser2.mly"
                     ( mk_ctor_by_name c e )
# 370 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (
# 14 "src/parser2.mly"
       (string)
# 377 "src/parser2.ml"
)) * _menhir_state) * _menhir_state * (Lang.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (c : (
# 14 "src/parser2.mly"
       (string)
# 383 "src/parser2.ml"
    ))), _), _, (e : (Lang.Expr.t))) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 109 "src/parser2.mly"
                     ( mk_ctor_by_name c e )
# 388 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exp_comma_list_one : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.Expr.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.Expr.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (es : (Lang.Expr.t list))), _, (e : (Lang.Expr.t))) = _menhir_stack in
        let _v : (Lang.Expr.t list) = 
# 129 "src/parser2.mly"
    ( e :: es )
# 476 "src/parser2.ml"
         in
        _menhir_goto_exp_comma_list_one _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 | MenhirState45 | MenhirState15 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Lang.Expr.t))) = _menhir_stack in
        let _v : (Lang.Expr.t list) = 
# 127 "src/parser2.mly"
    ( [e] )
# 486 "src/parser2.ml"
         in
        _menhir_goto_exp_comma_list_one _menhir_env _menhir_stack _menhir_s _v
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
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
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
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (es : (Lang.Expr.t list))), _, (e : (Lang.Expr.t))) = _menhir_stack in
        let _v : (Lang.Expr.t list * Lang.Expr.t) = 
# 80 "src/parser2.mly"
    ( (es,e) )
# 531 "src/parser2.ml"
         in
        _menhir_goto_example _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 | MenhirState19 ->
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
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState25 in
                let _v : (Lang.Expr.t list) = 
# 85 "src/parser2.mly"
    ( [] )
# 558 "src/parser2.ml"
                 in
                _menhir_goto_exp_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.Expr.t))) = _menhir_stack in
            let _v : (Lang.Expr.t list) = 
# 91 "src/parser2.mly"
    ( [e] )
# 571 "src/parser2.ml"
             in
            _menhir_goto_nonempty_exp_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 | MenhirState0 ->
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
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Lang.Expr.t))), _, (e2 : (Lang.Expr.t))) = _menhir_stack in
        let _v : (Lang.Expr.t list * Lang.Expr.t) = 
# 78 "src/parser2.mly"
    ( ([e1], e2) )
# 615 "src/parser2.ml"
         in
        _menhir_goto_example _menhir_env _menhir_stack _menhir_s _v
    | MenhirState41 ->
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
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | UID _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Lang.Expr.t))) = _menhir_stack in
        Obj.magic _1

and _menhir_reduce9 : _menhir_env -> ('ttv_tail * _menhir_state * (
# 14 "src/parser2.mly"
       (string)
# 661 "src/parser2.ml"
)) * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s, (c : (
# 14 "src/parser2.mly"
       (string)
# 667 "src/parser2.ml"
    ))), _) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 105 "src/parser2.mly"
    (
      mk_ctor_by_name c Expr.mk_unit
    )
# 674 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/parser2.mly"
       (string)
# 681 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (x : (
# 13 "src/parser2.mly"
       (string)
# 686 "src/parser2.ml"
  )) ->
    let _v : (Lang.Expr.t) = 
# 97 "src/parser2.mly"
    ( Expr.mk_var (Id.create x) )
# 691 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : (Lang.Expr.t) = 
# 115 "src/parser2.mly"
    ( mk_ctor_by_name "Nil" (Expr.mk_unit))
# 701 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/parser2.mly"
       (int)
# 708 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (c : (
# 18 "src/parser2.mly"
       (int)
# 713 "src/parser2.ml"
  )) ->
    let _v : (Lang.Expr.t) = 
# 99 "src/parser2.mly"
    ( Expr.from_int c )
# 718 "src/parser2.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "src/parser2.mly"
       (string)
# 725 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState1 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | LID _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s
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
# 14 "src/parser2.mly"
       (string)
# 769 "src/parser2.ml"
        ))) = _menhir_stack in
        let _v : (Lang.Expr.t) = 
# 101 "src/parser2.mly"
    (
      mk_ctor_by_name c Expr.mk_unit
    )
# 776 "src/parser2.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/parser2.mly"
       (string)
# 806 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState5 in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/parser2.mly"
       (int)
# 839 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
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

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "src/parser2.mly"
       (string)
# 914 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LBRACKET ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LID _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState40 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LID _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState41 in
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s
        | UID _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | UID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "src/parser2.mly"
       (string)
# 960 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState49 in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/parser2.mly"
       (int)
# 991 "src/parser2.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

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

and examples : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((Lang.Expr.t list * Lang.Expr.t) list) =
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
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAREN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UID _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and exp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lang.Expr.t) =
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
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LID _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | UID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
