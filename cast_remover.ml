open Ast
open Ast_mapper

let remove_cast tp =
  let rec can_remove s t =
    match s.t_desc, t.t_desc with
    | Ty_var _, Ty_var _ -> true
    | Ty_fun (s1, s2), Ty_fun (t1, t2) -> can_remove t1 s1 && can_remove s2 t2
    | Ty_const _, Ty_const _ -> true
    | Ty_refine (s', _, _), _ -> can_remove s' t
    | Ty_link s', _ -> can_remove s' t
    | _, Ty_link t' -> can_remove s t'
    | Ty_subst s', _ -> can_remove s' t
    | _, Ty_subst t' -> can_remove s t'
    | _ -> false
  in
  let m = {
    default with
    expression =
      fun iter e ->
        match e.e_desc with
        | Expr_cast _ -> assert false
        | Expr_dyn (e, t) ->
          let e' = iter.expression iter e in
          let t' = iter.ty iter t in
          if can_remove e'.e_ty t' then e' else {e_desc = Expr_dyn (e', t'); e_ty = e'.e_ty}
        | _ ->
          default.expression iter e
  }
  in
  m.top_phrase m tp
