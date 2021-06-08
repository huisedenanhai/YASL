open Syntax

type context = {
  ty_name : (string * ty_declare) list;
  var_ty : (string * ty) list;
}

let builtin_ctx = { ty_name = []; var_ty = [] }

let look_up_ty ctx var =
  List.find_map (fun (n, ty) -> if n = var then Some ty else None) ctx.var_ty

let type_toplevel_tm ctx tp_tm = (ctx, tp_tm)

let rec type_toplevel ctx = function
  | [] -> []
  | tp_tm :: tl ->
      let ctx, tp_tm = type_toplevel_tm ctx tp_tm in
      tp_tm :: type_toplevel ctx tl
