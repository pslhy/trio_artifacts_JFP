(*******************************************************************************
 * eval.ml - environment-based evaluator
 ******************************************************************************)

let eq = (=)

open Core
open Consts
open Lang
open Printf

exception Eval_error of string
let raise_eval_error s = raise (Eval_error s)

(***** Helpers {{{ *****)

let rec find_first_branch (c:id) (bs:branch list) : branch =
  match bs with
  | [] -> raise @@ Internal_error ("Constructor not found during matching: " ^ c)
  | ((c', x), e)::bs -> if eq c c' then ((c', x), e) else find_first_branch c bs

let rec extract_values_from_pattern (v:value) (p:pattern) : (id * value) list =
  match (p, v) with
  | (PWildcard, _) -> []
  | (PVar x,    _) -> [(x, v)]
  | (PTuple ps, VTuple vs) ->
    if List.length ps <> List.length vs then
      failwith (sprintf "extract_values_from_pattern: pattern %s does not match value %s"
                (Pp.pp_pattern p) (Pp.pp_value v))
    else
      List.concat_map ~f:(fun (v, p) -> extract_values_from_pattern v p) (List.zip_exn vs ps)
  | (PRcd ps, VRcd vs) ->
    begin try List.concat_map ~f:(fun (p, v) -> extract_values_from_pattern v p) (Util.zip_without_keys ps vs)
    with Invalid_argument _ ->
      failwith (sprintf "extract_values_from pattern: pattern %s does not match value %s"
                (Pp.pp_pattern p) (Pp.pp_value v))
    end
  | (_, _) -> 
      failwith (sprintf "extract_values_from_pattern: pattern %s does not match value %s"
                (Pp.pp_pattern p) (Pp.pp_value v))

(***** }}} *****)

(***** Evaluation Cache {{{ *****)

module GTS : sig
  type t = { env : env; e : exp }
  val make_key : env -> exp -> t
  include Hashable.S with type t := t
end = struct
  module T = struct
    type t = { env : env; e : exp }
    let make_key (env:env) (e:exp) = { env = env; e = e }
    let hash k = Hashtbl.hash k
    let hash_fold_t s k = Hash.fold_int s (hash k)
    let compare = compare
    let sexp_of_t (_:t) : Sexp.t = failwith "GTS.sexp_of_t unimplemented"
    let t_of_sexp (_:Sexp.t) : t = failwith "GTS.t_of_sexp unimplemented"
  end
  include T
  include Hashable.Make(T)
end

let lookup_tables : bool ref = ref true ;;
let memo_eval_tbl : (GTS.t, value) Hashtbl.t =
  GTS.Table.create ()

let find_in_table tbl key =
  if !eval_lookup_tables then
    Hashtbl.find tbl key
  else
    None

(***** }}} *****)

(* Evaluates e to a value under env *)
let rec eval (env:env) (e:exp) : value =
  (*let key = GTS.make_key env e in*)
  match None (*find_in_table memo_eval_tbl key*) with
  | Some ans -> ans
  | None ->
      let ans = begin match e with
      | EVar x -> List.Assoc.find_exn ~equal:String.equal env x
      | EApp (e1, e2) ->
            let (v1, v2) = (eval env e1, eval env e2) in
            begin match v1 with
            | VFun (x, e, closure) -> eval ((x, v2) :: !closure) e
            | VPFun vps ->
                begin match Util.find_first (fun (v1, _) -> eq v1 v2) vps with
                | Some (_, v) -> v
                | None ->
                    raise_eval_error @@ sprintf
                      "Non-matched value %s found with partial function:\n%s"
                        (Pp.pp_value v2) (Pp.pp_value v1)
                end
            | _ -> raise_eval_error ("Non-function value found in application" ^ (Pp.pp_value v1))
            end
      | EFun ((x, _), e) -> VFun (x, e, ref env)
      | ELet (f, is_rec, xs, t, e1, e2) ->
          let count = List.length xs in
          if count = 0 then
            (* Value binding *)
            let v1 = eval env e1 in
            eval ((f, v1) :: env) e2
          else
            (* Function binding *)
            let rec binding_to_funs xs e =
              match xs with
              | []      -> e
              | x :: xs -> EFun (x, binding_to_funs xs e)
            in
            let (x1, t1) = List.hd_exn xs in
            let fn = if is_rec then
              let e1 = binding_to_funs (List.tl_exn xs) e1 in
                EFix ( f, (x1, t1)
                     , (List.map ~f:snd (List.tl_exn xs)) @ [t] |> types_to_arr
                     , e1 )
            else
              binding_to_funs xs e1
            in
            eval ((f, eval env fn) :: env) e2
      | ECtor (c, e)  -> VCtor (c, eval env e)
      | ETuple es     -> VTuple (List.map ~f:(eval env) es)
      | ERcd es       -> VRcd (List.map ~f:(fun (l,e) -> (l,eval env e)) es)
      | EMatch (e, bs) ->
          let v = eval env e in
          begin match v with
          | VCtor (c, v) ->
              let ((_, p_opt), e) = find_first_branch c bs in
              begin match p_opt with
              | None -> eval env e
              | Some p -> eval ((extract_values_from_pattern v p) @ env) e
              end
          | _ ->
            raise_eval_error @@
              sprintf "Non-datatype value found in match: %s" (Pp.pp_exp e)
          end
      | EPFun ios ->
          VPFun (List.map ~f:(fun (e1, e2) -> (eval env e1, eval env e2)) ios)
      | EFix (f, (x, _), _, e) ->
          let closure = ref [] in
          let v = VFun (x, e, closure) in
          closure := (f, v) :: env; v
      | EProj (n, e) ->
          begin match eval env e with
          | VTuple vs -> List.nth_exn vs (n - 1)
          | _ -> raise_eval_error @@
                 sprintf "Non-tuple value found in projection: %s" (Pp.pp_exp e)
          end
      | ERcdProj (l, e) ->
        let v = eval env e in
        begin match v with
        | VRcd vs -> begin match Util.lookup l vs with
                      | Some v' -> v'
                      | None -> raise_eval_error @@
                                sprintf "Label '%s' not found in record: %s" l (Pp.pp_value v)
                      end
        | _ -> raise_eval_error @@
               sprintf "Non-record value found in projection: %s" (Pp.pp_exp e)
        end
      | EUnit -> VUnit
      end
  in
  (*(Hashtbl.set memo_eval_tbl ~key ~data:ans; ans)*) ans

(* Generates the initial synthesis environment. *)
let gen_init_env (ds:decl list) : env =
  let process env = function
    | DData _ -> env
    | DLet (f, is_rec, xs, t, e) ->
        if List.length xs = 0 then
          (* Value binding *)
          let v = eval env e in
          (f, v) :: env
        else
          (* Function binding *)
          let v = eval env (ELet (f, is_rec, xs, t, e, EVar f)) in
          (f, v) :: env
  in
    List.fold_left ~f:process ~init:[] ds

(* Generates the initial synthesis world. *)
let gen_init_world (env:env) (es:exp list) : world list =
  List.map ~f:(fun e -> ([], eval env e)) es
