open Lang
open Nondet.Syntax

(*******************************************************************************
 * Identifier generation
 *)

let fresh_ident gamma first_char =
  let extract_number (ident : string) : int option =
    let ident_len =
      String.length ident
    in
      if ident_len > 0 && Char.equal (String.get ident 0) first_char then
        ident
          |> StringLabels.sub ~pos:1 ~len:(ident_len - 1)
          |> int_of_string_opt
      else
        None
  in
  let fresh_number : int =
    gamma
      |> Type_ctx.names
      |> List.filter_map extract_number
      |> List2.maximum
      |> Option2.map ((+) 1)
      |> Option2.with_default 1
  in
    String.make 1 first_char ^ string_of_int fresh_number

let function_char =
  'f'

let variable_char =
  'x'

let match_char =
  'y'

(*******************************************************************************
 * Polymorphic helpers
 *)

let simple_types : datatype_ctx -> type_ctx -> typ Nondet.t =
  fun sigma gamma ->
    let polymorphic_variables =
      gamma
        |> Type_ctx.all_poly
        |> List.map (fun x -> TVar x)
    in
    (* Uncomment for monomorphic instantiations *)
    (*
    let monomorphic_datatypes =
      sigma
        |> List2.concat_map
             ( fun (_name, (type_params, _)) ->
                 if type_params = [] then
                   [TData (name, [])]
                 else
                   []
             )
    in
    *)
    let rank_zero_nd =
      Nondet.from_list
        ( polymorphic_variables
            (* @ monomorphic_datatypes *)
        )
    in
    let datatypes_nd =
      let* (datatype_name, datatype_params) =
        sigma
          |> List.map
               ( fun (name, (type_params, _)) ->
                   ( name
                   ,   type_params
                   )
               )
          |> Nondet.from_list
      in
      datatype_params
        |> List.map (fun _ -> rank_zero_nd)
        |> Nondet.one_of_each
        |> Nondet.map
             ( fun args ->
                 TData (datatype_name, args)
             )
    in
    Nondet.union
      [ Nondet.from_list polymorphic_variables
      ; datatypes_nd
      ]

let instantiations :
 datatype_ctx -> type_ctx -> string -> typ -> (typ * exp) Nondet.t =
  fun sigma gamma name tau ->
    match Type.peel_forall tau with
      | ([], _) ->
          Nondet.pure
            ( tau
            , EVar name
            )

      | (params, bound_type) ->
          let simple_types_nd =
            simple_types sigma gamma
          in
          params
            |> List.map (fun _ -> simple_types_nd)
            |> Nondet.one_of_each
            |> Nondet.map
                 ( fun args ->
                     ( Type.substitute_many
                         ~bindings:(List.combine params args)
                         bound_type
                     , Desugar.app
                         (EVar name)
                         ( List.map
                             (fun a -> EAType a)
                             args
                         )
                     )
                 )

(*******************************************************************************
 * Term permission helpers
 *)


type term_permission =
  | Must
  | May
  | Not

let parts (k : int) : term_permission list Nondet.t =
  let+ i =
    Nondet.from_list (List2.range ~low:1 ~high:k)
  in
    List2.repeat (i - 1) Not @ [Must] @ List2.repeat (k - i) May

(*******************************************************************************
 * Caching
 *)

(* Types *)

type term_kind =
  | E
  | I

type gen_input =
  { sigma : datatype_ctx
  ; term_kind : term_kind
  ; term_size : int
  ; rel_binding : type_binding option
  ; goal : gen_goal
  }

(* Hashing *)

let hash ({ term_kind; term_size; rel_binding; goal } : gen_input) : string =
  let rec hash_type (tau : typ) : string =
    match tau with
      | TArr (tau1, tau2) ->
          "[" ^ hash_type tau1 ^ ">" ^ hash_type tau2 ^ "]"

      | TTuple taus ->
          taus
            |> List.map hash_type
            |> String.concat ","
            |> (fun s -> "(" ^ s ^ ")")

      | TData (d, type_args) ->
          d ^ "<" ^ String.concat "," (List.map hash_type type_args)

      | TForall (a, bound_type) ->
          "f:" ^ a ^ "." ^ hash_type bound_type

      | TVar x ->
          x
  in
  let hash_bind_spec (bind_spec : bind_spec) : string =
    match bind_spec with
      | NoSpec -> "."
      | Rec name -> "r:" ^ name
      | Arg name -> "a:" ^ name
      | Dec name -> "d:" ^ name
  in
  let tk_string =
    match term_kind with
      | E -> "E"
      | I -> "I"
  in
  let ts_string =
    string_of_int term_size
  in
  let rb_string =
    match rel_binding with
      | None ->
          ""

      | Some (name, (tau, bind_spec)) ->
          name ^ ";" ^ hash_type tau ^ ";" ^ hash_bind_spec bind_spec
  in
  let goal_string =
    let (gamma, goal_type, goal_dec) =
      goal
    in
    (* Sigma never changes, so no need to keep track of it in the cache *)
    let gamma_type_string =
      gamma
        |> Type_ctx.all_type
        |> List.map
             ( fun (name, (tau, bind_spec)) ->
                 name ^ "`" ^ hash_type tau ^ "`" ^ hash_bind_spec bind_spec
             )
        |> String.concat "&"
    in
    let gamma_poly_string =
      gamma
        |> Type_ctx.all_poly
        |> List.map
             ( fun name ->
                 name
             )
        |> String.concat "&"
    in
    let gt_string =
      hash_type goal_type
    in
    let gd_string =
      Option2.with_default "." goal_dec
    in
      gamma_type_string ^ "!" ^ gamma_poly_string ^ "!"
        ^ gt_string ^ "!" ^ gd_string
  in
    tk_string ^ "!" ^ ts_string ^ "@" ^ rb_string ^ "#" ^ goal_string

(* Caching *)

let gen_cache : (string, exp Nondet.t) Hashtbl.t =
  Hashtbl.create 100

let lookup (gen_input : gen_input) : exp Nondet.t option =
  gen_input
    |> hash
    |> Hashtbl.find_opt gen_cache

let record (gen_input : gen_input) (solution : exp Nondet.t) : exp Nondet.t =
  Hashtbl.add gen_cache (hash gen_input) solution;
  solution

(*******************************************************************************
 * Term generation
 *)

(* --- Important info about the term generation helpers! ---
 *
 * Do NOT call gen_e, rel_gen_e, gen_i, or rel_gen_i from anywhere EXCEPT inside
 * the actual gen function. The gen function handles caching; no other code
 * should worry about directly manipulating the cache.
 *
 * So, if, for example, genE wants to recursively call itself, it should
 * actually do so indirectly via calling gen with the appropriate arguments.
 *
 * Also, these helpers assume term_size > 0.
 *)

let rec gen_e
  (sigma : datatype_ctx)
  (term_size : int)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    match Type_ctx.peel_type gamma with
      | Some (binding, gamma_rest) ->
          Nondet.union
            [ gen
                { sigma
                ; term_kind = E
                ; term_size
                ; rel_binding = Some binding
                ; goal = (gamma_rest, goal_type, goal_dec)
                }
            ; gen
                { sigma
                ; term_kind = E
                ; term_size
                ; rel_binding = None
                ; goal = (gamma_rest, goal_type, goal_dec)
                }
            ]

      | None ->
          Nondet.none

(* A helper for the application part of rel_gen_e *)
and rel_gen_e_app
  (sigma : datatype_ctx)
  (term_size : int)
  (rel_binding : type_binding)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    let* _ =
      Nondet.guard (Option.is_none goal_dec)
    in
    let combined_gamma =
      Type_ctx.add_type rel_binding gamma
    in
    let app_combine
     (head_nd : exp Nondet.t) (arg_nd : exp Nondet.t) : exp Nondet.t =
      let* head =
        head_nd
      in
      let special =
        match Type.bind_spec combined_gamma head with
          | Rec _ ->
              true

          | _ ->
              false
      in
      let* arg =
        arg_nd
      in
      let+ _ =
        Nondet.guard @@
          Type.structurally_decreasing combined_gamma ~head ~arg
      in
        EApp (special, head, EAExp arg)
    in
    let* arg_type =
      gamma
        |> Type_ctx.all_type
        |> List.map
             ( fun (name, (tau, _)) ->
                 let* (specialized_tau, _) =
                   instantiations sigma gamma name tau
                 in
                 Nondet.lift_option
                   ( Type.domain_of_codomain
                       ~codomain:goal_type
                       specialized_tau
                   )
             )
        |> Nondet.union
    in
    let* partition =
      Nondet.from_list @@
        Int2.partition_permutations
          ~n:(term_size - 1) (* -1 for application *)
          ~k:2
    in
      match partition with
        (* Will always happen*)
        | [k_head; k_arg] ->
            let head_goal =
              ( gamma
              , TArr (arg_type, goal_type)
              , None
              )
            in
            let arg_goal =
              ( gamma
              , arg_type
              , None
              )
            in
            let head_solution_nd =
              gen
                { sigma
                ; term_kind = E
                ; term_size = k_head
                ; rel_binding = None
                ; goal = head_goal
                }
            in
            let rel_head_solution_nd =
              gen
                { sigma
                ; term_kind = E
                ; term_size = k_head
                ; rel_binding = Some rel_binding
                ; goal = head_goal
                }
            in
            let arg_solution_nd =
              gen
                { sigma
                ; term_kind = I
                ; term_size = k_arg
                ; rel_binding = None
                ; goal = arg_goal
                }
            in
            let rel_arg_solution_nd =
              gen
                { sigma
                ; term_kind = I
                ; term_size = k_arg
                ; rel_binding = Some rel_binding
                ; goal = arg_goal
                }
            in
              Nondet.union @@
                [ app_combine rel_head_solution_nd arg_solution_nd
                ; app_combine head_solution_nd rel_arg_solution_nd
                ; app_combine rel_head_solution_nd rel_arg_solution_nd
                ]

        | _ ->
            Log.warn
              ( "integer partition is incorrect size (is "
              ^ string_of_int (List.length partition)
              ^ ", should be 2, called with n = "
              ^ string_of_int (term_size - 1)
              ^ ")"
              );
            Nondet.none

and rel_gen_e
  (sigma : datatype_ctx)
  (term_size : int)
  ((rel_name, (rel_type, rel_bind_spec)) as rel_binding : type_binding)
  ((gamma, goal_type, goal_dec) as goal : gen_goal)
  : exp Nondet.t =
    match term_size with
      | 1 ->
          let* (specialized_type, specialized_exp) =
            instantiations sigma gamma rel_name rel_type
          in
          if
            Type.matches goal_type specialized_type
              && Type.matches_dec goal_dec rel_bind_spec
          then
            Nondet.pure
              specialized_exp
          else if
            Type.matches_dec
              goal_dec
              (Type.sub_bind_spec rel_bind_spec)
          then
            (* "Focusing" *)
            begin match rel_type with
              | TTuple component_types ->
                  let n =
                    List.length component_types
                  in
                    component_types
                      |> List.mapi Pair2.pair
                      |> List.filter (snd >> Type.matches goal_type)
                      (* Should be 1-indexed, so use i + 1 *)
                      |> List.map
                           (fun (i, _) -> EProj (n, i + 1, EVar rel_name))
                      |> Nondet.from_list

              | _ ->
                  Nondet.none
            end
          else
            Nondet.none

      (* No unary operators *)
      | 2 ->
          Nondet.none

      (* All applications have size > 2 *)
      | _ ->
        rel_gen_e_app sigma term_size rel_binding goal

and genp_i
  (sigma : datatype_ctx)
  (term_size : int)
  (tp : term_permission)
  (rel_binding : type_binding)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    let (rel_binding', gamma') =
      match tp with
        | Must ->
            (Some rel_binding, gamma)

        | May ->
            (None, Type_ctx.add_type rel_binding gamma)

        | Not ->
            (None, gamma)
    in
      gen
        { sigma
        ; term_kind = I
        ; term_size
        ; rel_binding = rel_binding'
        ; goal = (gamma', goal_type, goal_dec)
        }

and gen_i
  (sigma : datatype_ctx)
  (term_size : int)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    let* _ =
      Nondet.guard (Option.is_none goal_dec)
    in
      match Type_ctx.peel_type gamma with
        | Some (binding, gamma_rest) ->
            Nondet.union
              [ gen
                  { sigma
                  ; term_kind = I
                  ; term_size
                  ; rel_binding = Some binding
                  ; goal = (gamma_rest, goal_type, goal_dec)
                  }
              ; gen
                  { sigma
                  ; term_kind = I
                  ; term_size
                  ; rel_binding = None
                  ; goal = (gamma_rest, goal_type, goal_dec)
                  }
              ]

        | None ->
            begin match goal_type with
              | TArr (tau1, tau2) ->
                  let f_name =
                    fresh_ident Type_ctx.empty function_char
                  in
                  let arg_name =
                    fresh_ident Type_ctx.empty variable_char
                  in
                  let+ body =
                    gen
                      { sigma
                      ; term_kind = I
                      ; term_size = term_size - 1 (* -1 for lambda *)
                      ; rel_binding = None
                      ; goal =
                          ( Type_ctx.concat_type
                              [ (arg_name, (tau1, Dec f_name))
                              ; (f_name, (goal_type, Rec f_name))
                              ]
                              Type_ctx.empty
                          , tau2
                          , None
                          )
                      }
                  in
                    EFix (Some f_name, PatParam (PVar arg_name), body)

              | TTuple taus ->
                  let tuple_size =
                    List.length taus
                  in
                  let* partition =
                    Nondet.from_list @@
                      Int2.partition_permutations
                        ~n:(term_size - 1) (* -1 for tuple *)
                        ~k:tuple_size
                  in
                    Nondet.map (fun es -> ETuple es) @@
                      Nondet.one_of_each @@
                        List.map2
                          begin fun tau n ->
                            gen
                              { sigma
                              ; term_kind = I
                              ; term_size = n
                              ; rel_binding = None
                              ; goal = (Type_ctx.empty, tau, None)
                              }
                          end
                          taus
                          partition

              | TData (datatype_name, datatype_args) ->
                  let* (ctor_name, arg_type) =
                    List.assoc_opt datatype_name sigma
                      |> Option2.map (snd >> Nondet.from_list)
                      |> Option2.with_default Nondet.none
                  in
                  let+ arg =
                    gen
                      { sigma
                      ; term_kind = I
                      ; term_size = term_size - 1 (* -1 for constructor *)
                      ; rel_binding = None
                      ; goal = (Type_ctx.empty, arg_type, None)
                      }
                  in
                    ECtor (ctor_name, datatype_args, arg)

              | TForall (a, bound_type) ->
                  let+ body =
                    gen
                      { sigma
                      ; term_kind = I
                      ; term_size = term_size - 1 (* -1 for lambda *)
                      ; rel_binding = None
                      ; goal =
                          ( Type_ctx.add_poly
                              a
                              Type_ctx.empty
                          , bound_type
                          , None
                          )
                      }
                  in
                    EFix (None, TypeParam a, body)

              | TVar _ ->
                  (* No introduction form for a type variable *)
                  Nondet.none
            end

and rel_gen_i
  (sigma : datatype_ctx)
  (term_size : int)
  (rel_binding : type_binding)
  ((gamma, goal_type, goal_dec) as goal : gen_goal)
  : exp Nondet.t =
    let* _ =
      Nondet.guard (Option.is_none goal_dec)
    in
    (* All E-forms are I-forms *)
    let e_option =
      gen
        { sigma
        ; term_kind = E
        ; term_size
        ; rel_binding = Some rel_binding
        ; goal
        }
    in
    let i_option =
      match goal_type with
        | TArr (tau1, tau2) ->
            let f_name =
              fresh_ident gamma function_char
            in
            let arg_name =
              fresh_ident gamma variable_char
            in
            let+ body =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for lambda *)
                ; rel_binding = Some rel_binding
                ; goal =
                    ( Type_ctx.concat_type
                        [ (arg_name, (tau1, Dec f_name))
                        ; (f_name, (goal_type, Rec f_name))
                        ]
                        gamma
                    , tau2
                    , None
                    )
                }
            in
              EFix (Some f_name, PatParam (PVar arg_name), body)

        | TTuple taus ->
            let tuple_size =
              List.length taus
            in
            let* partition =
              Nondet.from_list @@
                Int2.partition_permutations
                  ~n:(term_size - 1) (* -1 for tuple *)
                  ~k:tuple_size
            in
            let* part =
              parts tuple_size
            in
              Nondet.map (fun es -> ETuple es) @@
                Nondet.one_of_each @@
                  List2.map3
                    begin fun tau n tp ->
                      genp_i sigma n tp rel_binding (gamma, tau, None)
                    end
                    taus
                    partition
                    part

        | TData (datatype_name, datatype_args) ->
            let* (ctor_name, arg_type) =
              List.assoc_opt datatype_name sigma
                |> Option2.map (snd >> Nondet.from_list)
                |> Option2.with_default Nondet.none
            in
            let+ arg =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for constructor *)
                ; rel_binding = Some rel_binding
                ; goal = (gamma, arg_type, None)
                }
            in
              ECtor (ctor_name, datatype_args, arg)

        | TForall (a, bound_type) ->
            let+ body =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for lambda *)
                ; rel_binding = Some rel_binding
                ; goal =
                    ( Type_ctx.add_poly
                        a
                        gamma
                    , bound_type
                    , None
                    )
                }
            in
              EFix (None, TypeParam a, body)

        | TVar _ ->
            (* No introduction form for a type variable *)
            Nondet.none
    in
      Nondet.union [e_option; i_option]

and gen (gen_input : gen_input) : exp Nondet.t =
  if gen_input.term_size <= 0 then
    Nondet.none
  else
    match lookup gen_input with
      | Some solution ->
          solution

      | None ->
          let
            { term_size; sigma; goal; _ } =
              gen_input
          in
            record gen_input @@
              Nondet.dedup @@
                begin match (gen_input.term_kind, gen_input.rel_binding) with
                  | (E, None) ->
                      gen_e sigma term_size goal

                  | (E, Some rb) ->
                      rel_gen_e sigma term_size rb goal

                  | (I, None) ->
                      gen_i sigma term_size goal

                  | (I, Some rb) ->
                      rel_gen_i sigma term_size rb goal
                end

(*******************************************************************************
 * Term generation exports
 *)

let clear_cache _ =
  Hashtbl.reset gen_cache

let up_to_e sigma max_size goal =
  List2.range ~low:1 ~high:max_size
    |> List.map
         begin fun term_size ->
           gen
             { sigma
             ; term_kind = E
             ; rel_binding = None
             ; term_size
             ; goal
             }
         end
    |> Nondet.union
