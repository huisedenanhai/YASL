open Syntax

type glsl_ctx = string list * (string * (string * ty)) list

let ctx_add_glsl_struct_name ctx name =
  let tns, nbt = ctx in
  (name :: tns, nbt)

let ctx_has_glsl_struct_name ctx name =
  let tns, _ = ctx in
  match List.find_opt (fun s -> s = name) tns with
  | Some _ -> true
  | None -> false

let tuple_field_name n = Printf.sprintf "t%d" n

let rec glsl_name_of_plain_ty = function
  | TyNamed name -> name
  | TyTuple tys ->
      Printf.sprintf "Yasl__Tuple_%d"
        (Hashtbl.hash
           (tys |> List.map glsl_name_of_plain_ty |> String.concat "_"))

let glsl_name_of_ty = function
  | TyPlain pt -> glsl_name_of_plain_ty pt
  | TyArrow (id, _, _) ->
      Printf.sprintf "Yasl__TArrow_%s"
        (if id < 0 then "_" ^ string_of_int (-id) else string_of_int id)

let glsl_struct name fields =
  Printf.sprintf "struct %s {\n%s\n};" name
    (fields
    |> List.map (fun (k, t) ->
           Printf.sprintf "  %s %s;" (glsl_name_of_ty t) k)
    |> String.concat "\n")

let glsl_tuple_struct name tys =
  let kts = List.mapi (fun i t -> (tuple_field_name i, TyPlain t)) tys in
  glsl_struct name kts

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
      (ctx_add_glsl_struct_name ctx name, decls @ [ glsl_struct name kts ])
  | TyPlain (TyTuple tys) ->
      let ctx, decls = ensure_auto_glsl_plain_type_declares ctx tys in
      ( ctx_add_glsl_struct_name ctx name,
        decls @ [ glsl_tuple_struct name tys ] )
  | _ -> (ctx, [])

and ensure_auto_glsl_type_declares ctx tys =
  tys |> List.fold_left_map ensure_auto_glsl_type_declare ctx
  |> fun (ctx, decls) -> (ctx, List.flatten decls)

and ensure_auto_glsl_plain_type_declares ctx tys =
  tys |> List.map (fun t -> TyPlain t) |> ensure_auto_glsl_type_declares ctx

(* let glsl_function ctx arg arg_ty dst_ty = match dst_ty with | TyPlain
   dst_ty -> (* handle tuple arg/return types *) let _, _ =
   ensure_auto_glsl_plain_type_declares ctx [ arg_ty; dst_ty ] in () |
   TyArrow _ -> () *)

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
  | TopTmExtern _ -> (ctx, [])
  | TopTmTyDeclare (_, ty_decl) -> (
      match ty_decl with
      | TyDeclTuple (name, tys) ->
          let ctx, auto_decls =
            ensure_auto_glsl_plain_type_declares ctx tys
          in
          (ctx, auto_decls @ [ glsl_tuple_struct name tys ])
      | TyDeclRecord (name, kts) ->
          let _, tys = List.split kts in
          let ctx, auto_decls =
            ensure_auto_glsl_plain_type_declares ctx tys
          in
          let kts = List.map (fun (k, t) -> (k, TyPlain t)) kts in
          (ctx, auto_decls @ [ glsl_struct name kts ])
      | TyDeclOpaque _ -> (ctx, []))
  | TopTmLet _ -> (ctx, [])
  | TopTmEntry _ -> (ctx, [])

let gen_glsl toplevel emit_uniform =
  toplevel
  |> List.fold_left_map (fun t -> gen_glsl_tp_tm emit_uniform t) ([], [])
  |> (fun (_, ss) -> List.flatten ss)
  |> List.filter (fun s -> not (s = ""))
  |> String.concat "\n\n"
