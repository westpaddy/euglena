open Ast

module Types = struct
  let counter = ref 0
  let c_bool = {t_desc = Ty_const "%bool"; t_level = 1000}
  let c_int = {t_desc = Ty_const "%int"; t_level = 1000}
  let fresh_var () = incr counter; {t_desc = Ty_var ("a" ^ string_of_int !counter); t_level = 1000}
  let new_ty desc = {t_desc = desc; t_level = 1000}

  let rec repr t =
    match t.t_desc with
    | Ty_link t ->
      repr t
    | _ ->
      t

  let link t1 t2 =
    t1.t_desc <- Ty_link t2

  let iter f t =
    match t.t_desc with
    | Ty_var _ | Ty_const _ ->
      ()
    | Ty_fun (t1, t2) ->
      f t1; f t2
    | Ty_link t ->
      f t
end

module Env = struct
  let extend env x t = (x, t) :: env
  let lookup env x = List.assoc x env
end

exception Unify

let rec occur t1 t2 =
  if t1 = t2 then raise Unify else
    Types.iter (occur t1) t2

let rec unify t1 t2 =
  let t1 = Types.repr t1 in
  let t2 = Types.repr t2 in
  if t1 = t2 then () else
    let d1 = t1.t_desc in
    let d2 = t2.t_desc in
    match d1, d2 with
    | Ty_var _, _ ->
      occur t1 t2;
      Types.link t1 t2
    | _, Ty_var _ ->
      occur t2 t1;
      Types.link t2 t1
    | _ ->
      Types.link t1 t2;
      begin match d1, d2 with
        | Ty_fun (t1, t2), Ty_fun (s1, s2) ->
          unify t1 s1; unify t2 s2
        | Ty_const c1, Ty_const c2 when c1 = c2 ->
          ()
        | _ ->
          raise Unify
      end

let rec pattern env pat ty =
  let ref_env = ref env in
  let rec iter pat =
    let ty_pat ty = pat.p_ty <- Some ty; ty in
    match pat.p_desc with
    | Pat_var x ->
      let t = Types.fresh_var () in
      ref_env := Env.extend !ref_env x t;
      ty_pat t
  in
  unify ty (iter pat);
  !ref_env

let rec expression env expr =
  let ty_expr ty = expr.e_ty <- Some ty; ty in
  match expr.e_desc with
  | Expr_let (r, p, e1, e2) ->
    let rec_env = if r then pattern env p (Types.fresh_var ()) else env in
    let t1 = expression rec_env e1 in
    let env' = pattern env p t1 in
    let t2 = expression env' e2 in
    ty_expr t2
  | Expr_if (e1, e2, e3) ->
    let t1 = expression env e1 in
    let t2 = expression env e2 in
    let t3 = expression env e3 in
    unify t1 Types.c_bool;
    unify t2 t3;
    ty_expr t2
  | Expr_fun (p, e) ->
    let dom = Types.fresh_var () in
    let t = expression (pattern env p dom) e in
    ty_expr (Types.new_ty (Ty_fun (dom, t)))
  | Expr_app (e1, e2) ->
    let t1 = expression env e1 in
    let t2 = expression env e2 in
    let ran = Types.fresh_var () in
    unify t1 (Types.new_ty (Ty_fun (t2, ran)));
    ty_expr ran
  | Expr_var x ->
    ty_expr (Env.lookup env x)
  | Expr_int _ ->
    ty_expr Types.c_int
  | Expr_bool _ ->
    ty_expr Types.c_bool

let top_phrase env top =
  let ty_top ty = top.tp_ty <- Some ty; ty in
  match top.tp_desc with
  | Top_let (r, p, e) ->
    let rec_env = if r then pattern env p (Types.fresh_var ()) else env in
    let t = expression rec_env e in
    let env' = pattern env p t in
    (env', ty_top t)
  | Top_expr e ->
    (env, ty_top (expression env e))
