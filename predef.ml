open Ast

let ty_bool = Types.new_genty (Ty_const "bool")

let ty_int = Types.new_genty (Ty_const "int")

let env = List.fold_left (fun env (x, t) ->
  Env.extend env {i_desc = x; i_uid = -1; i_ty = t}
) Env.empty [
  ("%+", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_int)))));
  ("%-", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_int)))));
  ("%*", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_int)))));
  ("%/", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_int)))));
  ("%<", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_bool)))));
  ("%>", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_bool)))));
  ("%=", Types.new_genty (Ty_fun (ty_int, Types.new_genty (Ty_fun (ty_int, ty_bool)))));
]
