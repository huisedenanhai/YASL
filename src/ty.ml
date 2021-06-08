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

let builtin_ctx =
  {
    ops =
      [
        ("&&", [ binary_op_type_simple TyBool ]);
        ("||", [ binary_op_type_simple TyBool ]);
        ( "<",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
          ] );
        ( ">",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
          ] );
        ( "<=",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
          ] );
        ( ">=",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
          ] );
        ( "==",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
            binary_op_type_one TyBool TyBool;
          ] );
        ( "!=",
          [
            binary_op_type_one TyFloat TyBool;
            binary_op_type_one TyInt TyBool;
            binary_op_type_one TyBool TyBool;
          ] );
        ("+", [ binary_op_type_simple TyFloat; binary_op_type_simple TyInt ]);
        ( "-",
          [
            unary_op_type_simple TyFloat;
            unary_op_type_simple TyInt;
            binary_op_type_simple TyFloat;
            binary_op_type_simple TyInt;
          ] );
        ("*", [ binary_op_type_simple TyFloat; binary_op_type_simple TyInt ]);
        ("/", [ binary_op_type_simple TyFloat; binary_op_type_simple TyInt ]);
      ];
    ty_decls = [];
    var_ty = [];
  }

let ctx_add_var ctx name ty = { ctx with var_ty = (name, ty) :: ctx.var_ty }

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
          | IntLiteral _ -> TyInt
          | FloatLiteral _ -> TyFloat
          | BoolLiteral _ -> TyBool)
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
      if is_same_ty ty_cond (TyPlain TyBool) then
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
      | TyPlain (TyTuple [ TyBool; ty ]), TyPlain ty'
        when is_same_plain_ty ty ty' ->
          let ty = TyPlain ty in
          (TmLoop (info_set_type info ty, t1, t2), ty)
      | _ ->
          raise_type_err info
            (Printf.sprintf
               "loop body should return TyTuple[TyBool, %s], but found %s"
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
          let ty = TyPlain (TyCustom name) in
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
      | TyPlain (TyCustom name) -> (
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
          let ty = TyPlain (TyCustom name) in
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
      | TyPlain (TyCustom name) -> (
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

let type_toplevel_tm ctx tp_tm =
  match tp_tm with
  | TopTmUnfiorm (_, name, plain_ty) ->
      (ctx_add_var ctx name (TyPlain plain_ty), tp_tm)
  | TopTmExtern (_, name, ty) -> (ctx_add_var ctx name ty, tp_tm)
  | TopTmTyDeclare (info, ty_decl) ->
      (ctx_add_ty_decl info ctx ty_decl, tp_tm)
  | TopTmLet (info, name, tm) ->
      let tm, ty = type_tm ctx tm in
      (ctx_add_var ctx name ty, TopTmLet (info_set_type info ty, name, tm))
  | TopTmEntry (info, name, tm) ->
      let tm, ty = type_tm ctx tm in
      (ctx_add_var ctx name ty, TopTmEntry (info_set_type info ty, name, tm))

let rec type_toplevel ctx = function
  | [] -> []
  | tp_tm :: tl ->
      let ctx, tp_tm = type_toplevel_tm ctx tp_tm in
      tp_tm :: type_toplevel ctx tl
