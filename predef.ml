open Ast

let ty_bool = Types.new_ty (Ty_const "bool")

let ty_int = Types.new_ty (Ty_const "int")

let env = List.fold_left (fun env (x, t) -> Env.extend env x t) Env.empty [
    ("%+", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_int)))));
    ("%-", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_int)))));
    ("%*", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_int)))));
    ("%/", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_int)))));
    ("%<", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_bool)))));
    ("%>", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_bool)))));
    ("%=", Types.new_ty (Ty_fun (ty_int, Types.new_ty (Ty_fun (ty_int, ty_bool)))));
  ]
