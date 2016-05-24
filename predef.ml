open Ast

let ty_bool = Types.new_genty (Ty_const ("bool", []))

let ty_int = Types.new_genty (Ty_const ("int", []))

let cons_ty () =
  Types.incr_level ();
  let var1 = Types.fresh_var () in
  let var2 = Types.instantiate var1 in
  let lty1 = Types.new_ty (Ty_const ("list", [var2])) in
  let lty2 = Types.instantiate lty1 in
  let t = Types.new_ty (Ty_fun (var1, Types.new_ty (Ty_fun (lty1, lty2)))) in
  Types.decr_level ();
  Types.generalize t;
  t

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
  ("%::", cons_ty ())
]
