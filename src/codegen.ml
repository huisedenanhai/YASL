open Syntax

type glsl_ctx =
  (string * (string * ty) list) list * (string * (string * ty)) list

let ctx_add_glsl_struct ctx name fields =
  let tns, nbt = ctx in
  ((name, fields) :: tns, nbt)

let ctx_get_glsl_struct ctx name =
  let tns, _ = ctx in
  List.assoc_opt name tns

let ctx_has_glsl_struct_name ctx name =
  match ctx_get_glsl_struct ctx name with
  | Some _ -> true
  | None -> false

let ctx_add_var_binding ctx name mangled_name ty =
  let tns, nbt = ctx in
  (tns, (name, (mangled_name, ty)) :: nbt)

let glsl_func_call = "Yasl_call"

let glsl_ctx_name = "Yasl_ctx"

let tuple_field_name n = Printf.sprintf "t%d" n

let val_index = ref 0

let get_val_name name =
  let v = !val_index in
  val_index := v + 1;
  Printf.sprintf "Yasl_val_%s_%d" name v

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
  | TyArrow _ ->
      let _, nbt = ctx in
      let _, kts = List.split nbt in
      let _, tys = List.split kts in
      let ctx, decls = ensure_auto_glsl_type_declares ctx tys in
      let ctx, s_decl = glsl_add_struct ctx name kts in
      (ctx, decls @ s_decl)
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

let rec gen_glsl_extern_func ctx ty args native_name =
  match ty with
  | TyArrow (_, st, dt) ->
      let arg = "arg" in
      let arg_mangled = get_val_name "arg" in
      let _, nbt = ctx in
      let ctx, ty_decls =
        if is_arrow_ty dt then
          let ctx' = ctx_add_var_binding ctx arg arg_mangled (TyPlain st) in
          let (emitted_tys, _), ty_decls =
            gen_glsl_extern_func ctx' dt (args @ [ arg_mangled ]) native_name
          in
          ((emitted_tys, nbt), ty_decls)
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
                 match ctx_get_glsl_struct ctx ty_name with
                 | Some ctx_fields ->
                     ctx_fields
                     |> List.map (fun (s, _) ->
                            Printf.sprintf "  %s.%s = %s.%s;" next_ctx s
                              glsl_ctx_name s)
                     |> String.concat "\n"
                 | _ -> ""
               in
               Printf.sprintf "%s\n  %s.%s = %s;" copy_ctx next_ctx
                 arg_mangled arg_mangled)
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
  | TopTmExtern (_, _, ty, native_name) ->
      glsl_extern_func ctx ty native_name
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
  | TopTmLet _ -> (ctx, [])
  | TopTmEntry _ -> (ctx, [])

let gen_glsl toplevel emit_uniform =
  toplevel
  |> List.fold_left_map (fun t -> gen_glsl_tp_tm emit_uniform t) ([], [])
  |> (fun (_, ss) -> List.flatten ss)
  |> List.filter (fun s -> not (s = ""))
  |> String.concat "\n\n"
