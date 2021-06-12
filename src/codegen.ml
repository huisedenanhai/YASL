open Syntax
open Ty

exception CogenError of string

let raise_code_gen_error info = raise (CogenError info)

type glsl_ctx = {
  tns : (string * (string * ty) list) list;
  nbt : (string * (string * ty)) list;
}

let dump_glsl_ctx ctx =
  ctx.tns
  |> List.iter (fun (t, fields) ->
         Printf.printf "%s {\n%s\n}\n" t
           (fields
           |> List.map (fun (k, t) ->
                  Printf.sprintf "  %s: %s" k (desc_string_of_type t))
           |> String.concat "\n"))

let ctx_accept_emitted_ty ctx ctx' = { ctx with tns = ctx'.tns }

let ctx_add_glsl_struct ctx name fields =
  { ctx with tns = (name, fields) :: ctx.tns }

let ctx_get_glsl_struct ctx name = List.assoc_opt name ctx.tns

let ctx_has_glsl_struct_name ctx name =
  match ctx_get_glsl_struct ctx name with
  | Some _ -> true
  | None -> false

let ctx_get_var_binding ctx name = List.assoc name ctx.nbt

let ctx_add_var_binding ctx name mangled_name ty =
  { ctx with nbt = (name, (mangled_name, ty)) :: ctx.nbt }

let init_glsl_ctx = { tns = []; nbt = [] }

let glsl_func_call = "Yasl_call"

let glsl_ctx_name = "Yasl_ctx"

let tuple_field_name n = Printf.sprintf "t%d" n

let val_index = ref 0

let get_val_name name =
  let v = !val_index in
  val_index := v + 1;
  let name = Printf.sprintf "Yasl_val_%s_%d" name v in
  (* glsl does not allow __ in var name *)
  Str.global_replace (Str.regexp "_") "_1" name

let rec glsl_name_of_plain_ty = function
  | TyNamed name -> name
  | TyTuple tys ->
      Printf.sprintf "Yasl_Tuple_%d"
        (Hashtbl.hash
           (tys |> List.map glsl_name_of_plain_ty |> String.concat "_"))

let glsl_name_of_ty = function
  | TyPlain pt -> glsl_name_of_plain_ty pt
  | TyArrow (id, _, _) ->
      Printf.sprintf "Yasl_TArrow_%s"
        (if id < 0 then "_" ^ string_of_int (-id) else string_of_int id)

let glsl_add_struct ctx name fields =
  let decl =
    Printf.sprintf "struct %s {\n%s\n};" name
      (if List.length fields = 0 then "  float Yasl_placeholder;"
      else
        fields
        |> List.map (fun (k, t) ->
               Printf.sprintf "  %s %s;" (glsl_name_of_ty t) k)
        |> String.concat "\n")
  in
  (ctx_add_glsl_struct ctx name fields, [ decl ])

let glsl_add_tuple_struct ctx name tys =
  let kts = List.mapi (fun i t -> (tuple_field_name i, TyPlain t)) tys in
  glsl_add_struct ctx name kts

let auto_glsl_gen_name ty =
  match ty with
  | TyArrow (_, _, _) -> Some (glsl_name_of_ty ty)
  | TyPlain (TyTuple tys) -> Some (glsl_name_of_plain_ty (TyTuple tys))
  | _ -> None

let rec ensure_auto_glsl_type_declare ctx ty =
  match auto_glsl_gen_name ty with
  | Some name ->
      if ctx_has_glsl_struct_name ctx name then (ctx, [])
      else auto_glsl_gen_type name ctx ty
  | None -> (ctx, [])

and auto_glsl_gen_type name ctx ty =
  match ty with
  | TyArrow (_, st, dt) ->
      let ctx, decls =
        ensure_auto_glsl_type_declares ctx [ TyPlain st; dt ]
      in
      let nbt = ctx.nbt in
      let _, kts = List.split nbt in
      let _, tys = List.split kts in
      let ctx, decls' = ensure_auto_glsl_type_declares ctx tys in
      let ctx, s_decl = glsl_add_struct ctx name kts in
      (ctx, decls @ decls' @ s_decl)
  | TyPlain (TyTuple tys) ->
      let ctx, decls = ensure_auto_glsl_plain_type_declares ctx tys in
      let ctx, tp_decl = glsl_add_tuple_struct ctx name tys in
      (ctx, decls @ tp_decl)
  | _ -> (ctx, [])

and ensure_auto_glsl_type_declares ctx tys =
  tys |> List.fold_left_map ensure_auto_glsl_type_declare ctx
  |> fun (ctx, decls) -> (ctx, List.flatten decls)

and ensure_auto_glsl_plain_type_declares ctx tys =
  tys |> List.map (fun t -> TyPlain t) |> ensure_auto_glsl_type_declares ctx

let rec flatten_ty_arrow = function
  | TyArrow (_, st, dt) -> st :: flatten_ty_arrow dt
  | TyPlain ty -> [ ty ]

let glsl_fill_struct ctx dst_name src_name src_ty_name =
  match ctx_get_glsl_struct ctx src_ty_name with
  | Some ctx_fields ->
      ctx_fields
      |> List.map (fun (s, _) ->
             Printf.sprintf "  %s.%s = %s%s;" dst_name s
               (if src_name = "" then "" else src_name ^ ".")
               s)
  | _ -> []

let glsl_fill_struct_from_env ctx name ty_name =
  glsl_fill_struct ctx name "" ty_name

let glsl_fill_struct_with_values ctx name ty_name values =
  match ctx_get_glsl_struct ctx ty_name with
  | Some ctx_fields ->
      List.map2
        (fun (s, _) v -> Printf.sprintf "  %s.%s = %s;" name s v)
        ctx_fields values
  | _ -> []

let glsl_fill_struct_with_kvs name kvs =
  kvs |> List.map (fun (k, v) -> Printf.sprintf "  %s.%s = %s;" name k v)

let glsl_unpack_ctx ctx ty_name =
  match ctx_get_glsl_struct ctx ty_name with
  | Some ctx_fields ->
      ctx_fields
      |> List.map (fun (s, ty) ->
             Printf.sprintf "  %s %s = %s.%s;" (glsl_name_of_ty ty) s
               glsl_ctx_name s)
  | _ -> []

let force_unpack_ty_arrow = function
  | TyArrow (id, st, dt) -> (id, st, dt)
  | _ -> raise_code_gen_error "should be ty arrow"

let glsl_gen_func ctx ty arg_mangled body_glsl body_var =
  let _, st, dt = force_unpack_ty_arrow ty in
  let ty_name = glsl_name_of_ty ty in
  let st_name = glsl_name_of_plain_ty st in
  let dt_name = glsl_name_of_ty dt in
  Printf.sprintf "%s %s(%s %s, %s %s) {\n%s\n  return %s;\n}" dt_name
    glsl_func_call ty_name glsl_ctx_name st_name arg_mangled
    (glsl_unpack_ctx ctx ty_name @ [ "" ] @ body_glsl |> String.concat "\n")
    body_var

let ty_of_info = function
  | { ty = Some ty; _ } -> ty
  | _ -> raise_code_gen_error "term not typed"

let rec gen_glsl_extern_func (ctx : glsl_ctx) ty args native_name =
  match ty with
  | TyArrow (_, st, dt) ->
      let arg = "arg" in
      let arg_mangled = get_val_name "arg" in
      let nbt = ctx.nbt in
      let ctx, ty_decls =
        if is_arrow_ty dt then
          let ctx = ctx_add_var_binding ctx arg arg_mangled (TyPlain st) in
          let ctx, ty_decls =
            gen_glsl_extern_func ctx dt (args @ [ arg_mangled ]) native_name
          in
          ({ tns = ctx.tns; nbt }, ty_decls)
        else (ctx, [])
      in
      let ctx, ty_decls' = ensure_auto_glsl_type_declare ctx ty in
      let ty_decls = ty_decls @ ty_decls' in
      let dt_name = glsl_name_of_ty dt in
      let ty_name = glsl_name_of_ty ty in
      let st_name = glsl_name_of_ty (TyPlain st) in
      let func_template body =
        Printf.sprintf "%s %s(%s %s, %s %s) {\n%s\n}" dt_name glsl_func_call
          ty_name glsl_ctx_name st_name arg_mangled body
      in
      let body =
        if is_arrow_ty dt then
          let next_ctx = get_val_name "ctx" in
          let fill_ctx =
            Printf.sprintf "  %s %s;\n%s" dt_name next_ctx
              (let copy_ctx =
                 glsl_fill_struct ctx next_ctx glsl_ctx_name ty_name
               in
               copy_ctx
               @ [
                   Printf.sprintf "  %s.%s = %s;" next_ctx arg_mangled
                     arg_mangled;
                 ]
               |> String.concat "\n")
          in
          Printf.sprintf "%s\n  return %s;" fill_ctx next_ctx
        else
          Printf.sprintf "  return %s(%s);" native_name
            (let ctx_access =
               args |> List.map (Printf.sprintf "%s.%s" glsl_ctx_name)
             in
             String.concat ", " (ctx_access @ [ arg_mangled ]))
      in
      (ctx, ty_decls @ [ func_template body ])
  | _ -> (ctx, [])

let glsl_extern_func ctx ty native_name =
  let ty_list = flatten_ty_arrow ty in
  let ctx, ty_decls = ensure_auto_glsl_plain_type_declares ctx ty_list in
  let ctx, ty_decls' = gen_glsl_extern_func ctx ty [] native_name in
  (ctx, ty_decls @ ty_decls')

let gen_glsl_atom_value = function
  | IntLiteral v -> string_of_int v
  | FloatLiteral v -> string_of_float v
  | BoolLiteral v -> if v then "true" else "false"

let cast_plain = function
  | TyPlain t -> t
  | _ -> raise_code_gen_error "expect plain type"

let glsl_decl_val ty name =
  Printf.sprintf "  %s %s;" (glsl_name_of_ty ty) name

let ty_is_builtin_vec ty_name =
  all_builtin_vec_types |> List.exists (fun (s, _) -> s = ty_name)

let ty_is_builtin_matrix ty_name =
  all_builtin_matrix_types |> List.exists (fun (s, _) -> s = ty_name)

let ty_is_builtin_tuple ty_name =
  ty_is_builtin_vec ty_name || ty_is_builtin_matrix ty_name

let rec gen_glsl_tm ctx =
  let gen_abs ctx ty arg_name arg_ty body_tm =
    let arg_mangled = get_val_name arg_name in
    let body_var, body_glsl, ctx', body_decls =
      gen_glsl_tm
        (ctx_add_var_binding ctx arg_name arg_mangled (TyPlain arg_ty))
        body_tm
    in
    let ctx = ctx_accept_emitted_ty ctx ctx' in
    let ctx, ty_decls = ensure_auto_glsl_type_declare ctx ty in
    let func = glsl_gen_func ctx ty arg_mangled body_glsl body_var in
    let func_var = get_val_name "fn" in
    let ty_name = glsl_name_of_ty ty in
    let var_decl =
      Printf.sprintf "  %s %s;" ty_name func_var
      :: glsl_fill_struct_from_env ctx func_var ty_name
    in
    (func_var, var_decl, ctx, body_decls @ ty_decls @ [ func ])
  in
  let do_apply ctx ret_ty var1 var2 body decl =
    let res = get_val_name "app" in
    ( res,
      body
      @ [
          glsl_decl_val ret_ty res;
          Printf.sprintf "  %s = %s(%s, %s);" res glsl_func_call var1 var2;
        ],
      ctx,
      decl )
  in
  let init_tuple ctx ty tms =
    let ctx, decls = ensure_auto_glsl_type_declare ctx ty in
    let vars, body, ctx, decls =
      tms
      |> List.fold_left
           (fun (var, body, ctx, decl) tm ->
             let var', body', ctx', decl' = gen_glsl_tm ctx tm in
             (var @ [ var' ], body @ body', ctx', decl @ decl'))
           ([], [], ctx, decls)
    in
    let res = get_val_name "tp" in
    let ty_name = glsl_name_of_ty ty in
    let var_decl = glsl_decl_val ty res in
    let fill_tp =
      if ty_is_builtin_tuple ty_name then
        [
          Printf.sprintf "  %s = %s(%s);" res ty_name
            (String.concat ", " vars);
        ]
      else glsl_fill_struct_with_values ctx res ty_name vars
    in
    (res, body @ [ var_decl ] @ fill_tp, ctx, decls)
  in
  function
  | TmAtom (_, v) -> (gen_glsl_atom_value v, [], ctx, [])
  | TmAbs (info, arg_name, arg_ty, body_tm) ->
      gen_abs ctx (ty_of_info info) arg_name arg_ty body_tm
  | TmApp (info, t1, t2) ->
      let ty = ty_of_info info in
      let var1, body1, ctx, decl1 = gen_glsl_tm ctx t1 in
      let var2, body2, ctx, decl2 = gen_glsl_tm ctx t2 in
      do_apply ctx ty var1 var2 (body1 @ body2) (decl1 @ decl2)
  | TmLet (info, name, t1, t2) ->
      let ty1 = cast_plain (ty_of_info (get_tm_info t1)) in
      let ty2 = ty_of_info (get_tm_info t2) in
      let var1, body1, ctx, decl1 = gen_glsl_tm ctx t1 in
      let var2, body2, ctx, decl2 =
        gen_abs ctx (TyArrow (id_of_tm_info info, ty1, ty2)) name ty1 t2
      in
      do_apply ctx ty2 var2 var1 (body1 @ body2) (decl1 @ decl2)
  | TmIf (info, tc, t1, t2) ->
      let ty = ty_of_info info in
      let varc, bodyc, ctx, declc = gen_glsl_tm ctx tc in
      let var1, body1, ctx, decl1 = gen_glsl_tm ctx t1 in
      let var2, body2, ctx, decl2 = gen_glsl_tm ctx t2 in
      let res = get_val_name "if" in
      let decl_res = glsl_decl_val ty res in
      let concat = String.concat "\n" in
      let body =
        Printf.sprintf
          "  if (%s) {\n%s\n    %s = %s;\n  } else {\n%s\n    %s = %s;\n  }"
          varc (concat body1) res var1 (concat body2) res var2
      in
      (res, bodyc @ [ decl_res; body ], ctx, declc @ decl1 @ decl2)
  | TmLoop (info, t1, t2) ->
      let ty = ty_of_info info in
      let var1, body1, ctx, decl1 = gen_glsl_tm ctx t1 in
      let var2, body2, ctx, decl2 = gen_glsl_tm ctx t2 in
      let loop_cond = get_val_name "loop_cond" in
      let res = get_val_name "loop" in
      let var_decls =
        [
          Printf.sprintf "  %s %s = true;" "bool" loop_cond;
          glsl_decl_val ty res;
          Printf.sprintf "  %s = %s;" res var1;
        ]
      in
      let ty2 = ty_of_info (get_tm_info t2) in
      let _, _, dt = force_unpack_ty_arrow ty2 in
      let loop_body_res, loop_body, ctx, decl =
        do_apply ctx dt var2 res [] []
      in
      let loop =
        Printf.sprintf "  while (%s) {\n%s\n  %s = %s.%s;\n  %s = %s.%s;\n}"
          loop_cond
          (String.concat "\n" loop_body)
          loop_cond loop_body_res (tuple_field_name 0) res loop_body_res
          (tuple_field_name 1)
      in
      (res, body1 @ body2 @ var_decls @ [ loop ], ctx, decl1 @ decl2 @ decl)
  | TmTuple (info, tms) ->
      let ty = ty_of_info info in
      init_tuple ctx ty tms
  | TmNamedTuple (info, _, tms) ->
      let ty = ty_of_info info in
      init_tuple ctx ty tms
  | TmTupleAccess (info, tm, index) ->
      let ty = ty_of_info info in
      let var, body, ctx, decl = gen_glsl_tm ctx tm in
      let res = get_val_name "tp_access" in
      let var_decl = glsl_decl_val ty res in
      let tm_ty_name = glsl_name_of_ty (ty_of_info (get_tm_info tm)) in
      let do_access =
        if ty_is_builtin_vec tm_ty_name then
          let fields = [ "x"; "y"; "z"; "w" ] in
          Printf.sprintf "  %s = %s.%s;" res var (List.nth fields index)
        else if ty_is_builtin_matrix tm_ty_name then
          Printf.sprintf "  %s = %s[%d];" res var index
        else Printf.sprintf "  %s = %s.%s;" res var (tuple_field_name index)
      in
      (res, body @ [ var_decl; do_access ], ctx, decl)
  | TmRecord (info, _, kvs) ->
      let ty = ty_of_info info in
      let kvs, body, ctx, decls =
        kvs
        |> List.fold_left
             (fun (var, body, ctx, decl) (k, v) ->
               let var', body', ctx', decl' = gen_glsl_tm ctx v in
               (var @ [ (k, var') ], body @ body', ctx', decl @ decl'))
             ([], [], ctx, [])
      in
      let res = get_val_name "rcd" in
      let var_decl = glsl_decl_val ty res in
      let fill_rcd = glsl_fill_struct_with_kvs res kvs in
      (res, body @ [ var_decl ] @ fill_rcd, ctx, decls)
  | TmRecordAccess (info, tm, key) ->
      let ty = ty_of_info info in
      let var, body, ctx, decl = gen_glsl_tm ctx tm in
      let res = get_val_name "rcd_access" in
      let var_decl = glsl_decl_val ty res in
      let assign_val = Printf.sprintf "  %s = %s.%s;" res var key in
      (res, body @ [ var_decl; assign_val ], ctx, decl)
  | TmOp (info, op, tm) ->
      (* TODO: short cut for binary ops *)
      let ty = ty_of_info info in
      let var, body, ctx, decl = gen_glsl_tm ctx tm in
      let res = get_val_name "op" in
      let tm_ty = ty_of_info (get_tm_info tm) in
      let is_binary =
        match tm_ty with
        | TyPlain (TyTuple _) -> true
        | _ -> false
      in
      let res_decl = glsl_decl_val ty res in
      let do_op =
        if is_binary then
          Printf.sprintf "  %s = %s.%s %s %s.%s;" res var
            (tuple_field_name 0) op var (tuple_field_name 1)
        else Printf.sprintf "  %s = %s%s;" res op var
      in
      (res, body @ [ res_decl; do_op ], ctx, decl)
  | TmIdent (_, name) ->
      let name_mangled, _ = ctx_get_var_binding ctx name in
      (name_mangled, [], ctx, [])

let gen_glsl_tp_tm emit_uniform ctx = function
  | TopTmUnfiorm (_, name, pt) ->
      if emit_uniform then
        let ctx, auto_decls =
          ensure_auto_glsl_plain_type_declares ctx [ pt ]
        in
        ( ctx,
          auto_decls
          @ [
              Printf.sprintf "uniform %s %s;" (glsl_name_of_plain_ty pt) name;
            ] )
      else (ctx, [])
  | TopTmExtern (_, name, ty, native_name) ->
      let ctx, glsls = glsl_extern_func ctx ty native_name in
      let var_name = get_val_name name in
      let ty_name = glsl_name_of_ty ty in
      let func_decl = Printf.sprintf "%s %s;" ty_name var_name in
      (ctx_add_var_binding ctx name var_name ty, glsls @ [ func_decl ])
  | TopTmTyDeclare (_, ty_decl) -> (
      match ty_decl with
      | TyDeclTuple (name, tys) ->
          let ctx, auto_decls =
            ensure_auto_glsl_plain_type_declares ctx tys
          in
          let ctx, tp_decl = glsl_add_tuple_struct ctx name tys in
          (ctx, auto_decls @ tp_decl)
      | TyDeclRecord (name, kts) ->
          let _, tys = List.split kts in
          let ctx, auto_decls =
            ensure_auto_glsl_plain_type_declares ctx tys
          in
          let kts = List.map (fun (k, t) -> (k, TyPlain t)) kts in
          let ctx, s_decl = glsl_add_struct ctx name kts in
          (ctx, auto_decls @ s_decl)
      | TyDeclOpaque _ -> (ctx, []))
  | TopTmLet (info, name, tm) ->
      let ty = ty_of_info info in
      let vn, _, ctx, glsl = gen_glsl_tm ctx tm in
      (ctx_add_var_binding ctx name vn ty, glsl)
  | TopTmEntry (info, name, tm) ->
      let ty = ty_of_info info in
      let vn, _, ctx, glsl = gen_glsl_tm ctx tm in
      (ctx_add_var_binding ctx name vn ty, glsl)

let gen_glsl toplevel emit_uniform =
  toplevel
  |> List.fold_left_map
       (fun t -> gen_glsl_tp_tm emit_uniform t)
       init_glsl_ctx
  |> (fun (_, ss) -> List.flatten ss)
  |> List.filter (fun s -> not (s = ""))
  |> String.concat "\n\n"
