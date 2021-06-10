open Syntax

type context = {
  ops : (string * ty list) list;
  ty_decls : (string * ty_declare) list;
  var_ty : (string * ty) list;
}

exception TypeError of string

let raise_type_err tm_info desc =
  raise (TypeError (string_of_tm_info tm_info ^ ": " ^ desc))

let unary_op_type lhs ret = TyArrow (0, lhs, TyPlain ret)

let unary_op_type_simple ty = unary_op_type ty ty

let binary_op_type lhs rhs ret =
  TyArrow (0, TyTuple [ lhs; rhs ], TyPlain ret)

let binary_op_type_simple ty = binary_op_type ty ty ty

let binary_op_type_one arg_ty ret = binary_op_type arg_ty arg_ty ret

let rec repeat_n elem n = if n = 0 then [] else elem :: repeat_n elem (n - 1)

let builtin_vec_types prefix elem_ty =
  [ 2; 3; 4 ]
  |> List.map (fun n ->
         let name = Printf.sprintf "%svec%d" prefix n in
         (name, TyDeclTuple (name, repeat_n elem_ty n)))

let builtin_matrix_types prefix =
  [ 2; 3; 4 ]
  |> List.map (fun a -> [ 2; 3; 4 ] |> List.map (fun b -> (a, b)))
  |> List.flatten
  |> List.map (fun (a, b) ->
         let name =
           if a = b then Printf.sprintf "%smat%d" prefix a
           else Printf.sprintf "%smat%dx%d" prefix a b
         in
         ( name,
           TyDeclTuple
             (name, repeat_n (TyNamed (Printf.sprintf "%svec%d" prefix b)) a)
         ))

let ty_bool = TyNamed "bool"

let ty_int = TyNamed "int"

let ty_float = TyNamed "float"

let builtin_primitive_tys =
  [ "bool"; "int"; "float" ] |> List.map (fun n -> (n, TyDeclOpaque n))

let builtin_ctx =
  {
    ops =
      [
        ("&&", [ binary_op_type_simple ty_bool ]);
        ("||", [ binary_op_type_simple ty_bool ]);
        ( "<",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
          ] );
        ( ">",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
          ] );
        ( "<=",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
          ] );
        ( ">=",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
          ] );
        ( "==",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
            binary_op_type_one ty_bool ty_bool;
          ] );
        ( "!=",
          [
            binary_op_type_one ty_float ty_bool;
            binary_op_type_one ty_int ty_bool;
            binary_op_type_one ty_bool ty_bool;
          ] );
        ( "+",
          [ binary_op_type_simple ty_float; binary_op_type_simple ty_int ] );
        ( "-",
          [
            unary_op_type_simple ty_float;
            unary_op_type_simple ty_int;
            binary_op_type_simple ty_float;
            binary_op_type_simple ty_int;
          ] );
        ( "*",
          [ binary_op_type_simple ty_float; binary_op_type_simple ty_int ] );
        ( "/",
          [ binary_op_type_simple ty_float; binary_op_type_simple ty_int ] );
      ];
    ty_decls =
      builtin_primitive_tys
      @ builtin_vec_types "" ty_float
      @ builtin_vec_types "i" ty_int
      @ builtin_matrix_types ""
      @ builtin_matrix_types "i";
    var_ty = [];
  }

let ctx_add_var ctx name ty =
  if name = "_" then ctx else { ctx with var_ty = (name, ty) :: ctx.var_ty }

let ctx_add_ty_decl info ctx ty_decl =
  let ty_decls = ctx.ty_decls in
  let ty_name = name_of_ty_decl ty_decl in
  if List.mem_assoc ty_name ty_decls then
    raise_type_err info ("redefine type " ^ ty_name)
  else { ctx with ty_decls = (ty_name, ty_decl) :: ty_decls }

let look_up_ty_of_var_opt ctx var = List.assoc_opt var ctx.var_ty

let look_up_ty_of_var info ctx var =
  match look_up_ty_of_var_opt ctx var with
  | Some ty -> ty
  | None -> raise_type_err info (Printf.sprintf "variable %s not found" var)

let look_up_ty_declare_opt ctx ty_name = List.assoc_opt ty_name ctx.ty_decls

let look_up_ty_declare info ctx ty_name =
  match look_up_ty_declare_opt ctx ty_name with
  | Some decl -> decl
  | None -> raise_type_err info ("unknown type name " ^ ty_name)

(* ty1: S -> T ty2: S *)
let check_apply_opt ty1 ty2 =
  match ty1 with
  | TyArrow (_, st, dt) when is_same_ty (TyPlain st) ty2 -> Some dt
  | _ -> None

let check_apply info ty1 ty2 =
  match check_apply_opt ty1 ty2 with
  | Some dt -> dt
  | None ->
      raise_type_err info
        ("can not apply "
        ^ desc_string_of_type ty2
        ^ " to "
        ^ desc_string_of_type ty1)

let resolve_op_overload info ctx op ty =
  match List.assoc_opt op ctx.ops with
  | Some tys -> (
      match List.find_map (fun op_ty -> check_apply_opt op_ty ty) tys with
      | Some dt -> dt
      | None ->
          raise_type_err info
            (Printf.sprintf "no known overloading of %s for op %s"
               (desc_string_of_type ty) op))
  | None -> raise_type_err info ("unknown operator " ^ op)

let rec type_tm ctx tm =
  match tm with
  | TmAtom (info, v) ->
      let ty =
        TyPlain
          (match v with
          | IntLiteral _ -> ty_int
          | FloatLiteral _ -> ty_float
          | BoolLiteral _ -> ty_bool)
      in
      (TmAtom (info_set_type info ty, v), ty)
  | TmAbs (info, name, pt, tm) ->
      let ctx' = ctx_add_var ctx name (TyPlain pt) in
      let tm, ty = type_tm ctx' tm in
      let ty = TyArrow (id_of_tm_info info, pt, ty) in
      (TmAbs (info_set_type info ty, name, pt, tm), ty)
  | TmApp (info, t1, t2) ->
      let t1, ty1 = type_tm ctx t1 in
      let t2, ty2 = type_tm ctx t2 in
      let ty = check_apply info ty1 ty2 in
      (TmApp (info_set_type info ty, t1, t2), ty)
  | TmLet (info, name, t1, t2) ->
      let t1, ty1 = type_tm ctx t1 in
      let ctx' = ctx_add_var ctx name ty1 in
      let t2, ty2 = type_tm ctx' t2 in
      let ty = ty2 in
      (TmLet (info_set_type info ty, name, t1, t2), ty)
  | TmIf (info, tc, t1, t2) ->
      let tc, ty_cond = type_tm ctx tc in
      let t1, ty1 = type_tm ctx t1 in
      let t2, ty2 = type_tm ctx t2 in
      if is_same_ty ty_cond (TyPlain ty_bool) then
        if is_same_ty ty1 ty2 then
          let ty = ty1 in
          (TmIf (info_set_type info ty, tc, t1, t2), ty)
        else
          raise_type_err info
            (Printf.sprintf
               "two branches of if should have same type. ty1 = %s, ty2 = %s"
               (desc_string_of_type ty1) (desc_string_of_type ty2))
      else
        raise_type_err info
          ("if condition should have type bool. found "
          ^ desc_string_of_type ty_cond)
  | TmLoop (info, t1, t2) -> (
      let t1, ty1 = type_tm ctx t1 in
      let t2, ty2 = type_tm ctx t2 in
      (* notice the apply order *)
      let ty = check_apply info ty2 ty1 in
      (* constrain on the return type *)
      match (ty, ty1) with
      | TyPlain (TyTuple [ TyNamed "bool"; ty ]), TyPlain ty'
        when is_same_plain_ty ty ty' ->
          let ty = TyPlain ty in
          (TmLoop (info_set_type info ty, t1, t2), ty)
      | _ ->
          raise_type_err info
            (Printf.sprintf
               "loop body should return TyTuple[TyNamed(bool), %s], but \
                found %s"
               (desc_string_of_type ty1) (desc_string_of_type ty)))
  | TmTuple (info, tms) ->
      let tms, tys = List.split (type_tm_list ctx tms) in
      let ty = TyPlain (TyTuple tys) in
      (TmTuple (info_set_type info ty, tms), ty)
  | TmNamedTuple (info, name, tms) -> (
      match look_up_ty_declare info ctx name with
      | TyDeclTuple (_, decl_tys) ->
          let tm_tys = type_tm_list ctx tms in
          List.iter2
            (fun ty1 (t, ty2) ->
              if is_same_plain_ty ty1 ty2 then ()
              else
                raise_type_err (get_tm_info t)
                  (Printf.sprintf
                     "named tuple type mismatch, expect %s, found %s"
                     (desc_string_of_plain_ty ty1)
                     (desc_string_of_plain_ty ty2)))
            decl_tys tm_tys;
          let ty = TyPlain (TyNamed name) in
          let tms, _ = List.split tm_tys in
          (TmNamedTuple (info_set_type info ty, name, tms), ty)
      | _ ->
          raise_type_err info
            ("expect named tuple, found named record " ^ name))
  | TmTupleAccess (info, tm, index) -> (
      let tm, ty = type_tm ctx tm in
      let do_access tys =
        try
          let ty = TyPlain (List.nth tys index) in
          (TmTupleAccess (info_set_type info ty, tm, index), ty)
        with _ ->
          raise_type_err info
            (Printf.sprintf "tuple index %d out of bound for type %s" index
               (desc_string_of_type ty))
      in
      let fail _ =
        raise_type_err info
          ("can only index tuple or named tuple, but found type "
          ^ desc_string_of_type ty)
      in
      match ty with
      | TyPlain (TyTuple tys) -> do_access tys
      | TyPlain (TyNamed name) -> (
          match look_up_ty_declare info ctx name with
          | TyDeclTuple (_, tys) -> do_access tys
          | _ -> fail ())
      | _ -> fail ())
  | TmRecord (info, name, kvs) -> (
      match look_up_ty_declare info ctx name with
      | TyDeclRecord (_, decl_kts) ->
          let lbs, tms = List.split kvs in
          let tm_tys = type_tm_list ctx tms in
          List.iter2
            (fun lb (t, ty2) ->
              try
                let ty_decl = List.assoc lb decl_kts in
                if is_same_plain_ty ty_decl ty2 then ()
                else
                  raise_type_err (get_tm_info t)
                    (Printf.sprintf
                       "type mismatch for key %s in record %s, expect %s, \
                        found %s"
                       lb name
                       (desc_string_of_plain_ty ty_decl)
                       (desc_string_of_plain_ty ty2))
              with Not_found ->
                raise_type_err (get_tm_info t)
                  (Printf.sprintf "key %s not found for record %s" lb name))
            lbs tm_tys;
          List.iter
            (fun (decl_lb, _) ->
              if List.mem_assoc decl_lb kvs then ()
              else
                raise_type_err info
                  (Printf.sprintf "key %s not initialized for record %s"
                     decl_lb name))
            decl_kts;
          let ty = TyPlain (TyNamed name) in
          let tms, _ = List.split tm_tys in
          (TmRecord (info_set_type info ty, name, List.combine lbs tms), ty)
      | _ ->
          raise_type_err info
            ("expect named record, found named tuple " ^ name))
  | TmRecordAccess (info, tm, key) -> (
      let tm, ty = type_tm ctx tm in
      let fail _ =
        raise_type_err info
          ("expect record, found %s" ^ desc_string_of_type ty)
      in
      match ty with
      | TyPlain (TyNamed name) -> (
          let ty_decl = look_up_ty_declare info ctx name in
          match ty_decl with
          | TyDeclRecord (_, kts) -> (
              try
                let ty = TyPlain (List.assoc key kts) in
                (TmRecordAccess (info_set_type info ty, tm, key), ty)
              with Not_found ->
                raise_type_err info
                  (Printf.sprintf "key %s not found for record type %s" key
                     name))
          | _ -> fail ())
      | _ -> fail ())
  | TmIdent (info, name) ->
      let ty = look_up_ty_of_var info ctx name in
      (TmIdent (info_set_type info ty, name), ty)
  | TmOp (info, op, tm) ->
      let tm, ty = type_tm ctx tm in
      let ty = resolve_op_overload info ctx op ty in
      (TmOp (info_set_type info ty, op, tm), ty)

and type_tm_list ctx tms =
  tms
  |> List.map (fun t ->
         let t, ty = type_tm ctx t in
         match ty with
         | TyPlain ty -> (t, ty)
         | _ ->
             raise_type_err (get_tm_info t)
               ("elements in tuple should have plain types. found "
               ^ desc_string_of_type ty))

let rec check_plain_type_valid info ctx = function
  | TyNamed name ->
      let _ = look_up_ty_declare info ctx name in
      ()
  | TyTuple tys -> List.iter (fun t -> check_plain_type_valid info ctx t) tys

let rec check_type_valid info ctx = function
  | TyPlain plain_ty -> check_plain_type_valid info ctx plain_ty
  | TyArrow (_, st, dt) ->
      check_plain_type_valid info ctx st;
      check_type_valid info ctx dt

let check_ty_declare_valid info ctx = function
  | TyDeclTuple (_, tys) ->
      List.iter (fun t -> check_plain_type_valid info ctx t) tys
  | TyDeclRecord (_, kts) ->
      let rec check_kts checked = function
        | [] -> ()
        | (k, t) :: tl ->
            if List.mem_assoc k checked then
              raise_type_err info
                (Printf.sprintf "duplicated key '%s' in type declare" k)
            else (
              check_plain_type_valid info ctx t;
              check_kts ((k, t) :: checked) tl)
      in
      check_kts [] kts
  | TyDeclOpaque _ -> ()

let type_toplevel_tm ctx tp_tm =
  match tp_tm with
  | TopTmUnfiorm (info, name, plain_ty) ->
      check_plain_type_valid info ctx plain_ty;
      let ty = TyPlain plain_ty in
      ( ctx_add_var ctx name ty,
        TopTmUnfiorm (info_set_type info ty, name, plain_ty) )
  | TopTmExtern (info, name, ty) ->
      check_type_valid info ctx ty;
      if is_arrow_ty ty then
        ( ctx_add_var ctx name ty,
          TopTmExtern (info_set_type info ty, name, ty) )
      else raise_type_err info "extern var should have function type"
  | TopTmTyDeclare (info, ty_decl) ->
      check_ty_declare_valid info ctx ty_decl;
      let ty = TyPlain (TyNamed (name_of_ty_decl ty_decl)) in
      ( ctx_add_ty_decl info ctx ty_decl,
        TopTmTyDeclare (info_set_type info ty, ty_decl) )
  | TopTmLet (info, name, tm) ->
      let tm, ty = type_tm ctx tm in
      if is_arrow_ty ty then
        (ctx_add_var ctx name ty, TopTmLet (info_set_type info ty, name, tm))
      else raise_type_err info "toplevel let should have function type"
  | TopTmEntry (info, name, tm) ->
      let tm, ty = type_tm ctx tm in
      if is_arrow_ty ty then
        ( ctx_add_var ctx name ty,
          TopTmEntry (info_set_type info ty, name, tm) )
      else raise_type_err info "entry should have function type"

let rec type_toplevel ctx = function
  | [] -> []
  | tp_tm :: tl ->
      let ctx, tp_tm = type_toplevel_tm ctx tp_tm in
      tp_tm :: type_toplevel ctx tl
