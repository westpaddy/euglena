open Ast

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
      try
        begin match d1, d2 with
          | Ty_fun (t1, t2), Ty_fun (s1, s2) ->
            unify t1 s1; unify t2 s2
          | Ty_const c1, Ty_const c2 when c1 = c2 ->
            ()
          | _ ->
            raise Unify
        end
      with Unify ->
        t1.t_desc <- d1;
        raise Unify

let rec type_expr te =
  let ty_te ty = te.te_ty <- Some ty; ty in
  match te.te_desc with
  | Type_var x ->
    ty_te (Types.new_ty (Ty_var x))
  | Type_fun (te1, te2) ->
    ty_te (Types.new_ty (Ty_fun (type_expr te1, type_expr te2)))
  | Type_const x ->
    ty_te (Types.new_ty (Ty_const x))

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
    let env' = fst (let_bind env r p e1) in
    let t2 = expression env' e2 in
    ty_expr t2
  | Expr_if (e1, e2, e3) ->
    let t1 = expression env e1 in
    let t2 = expression env e2 in
    let t3 = expression env e3 in
    unify t1 Predef.ty_bool;
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
    ty_expr Predef.ty_int
  | Expr_bool _ ->
    ty_expr Predef.ty_bool

and let_bind env is_rec pat expr =
  Types.incr_level ();
  let pat_t = Types.fresh_var () in
  let rec_env = if is_rec then pattern env pat pat_t else env in
  let t = expression rec_env expr in
  unify pat_t t;
  let env' = pattern env pat pat_t in
  Types.decr_level ();
  (env', t)

let top_phrase env top =
  let ty_top ty = top.tp_ty <- Some ty; ty in
  match top.tp_desc with
  | Top_let (r, p, e) ->
    let env', t = let_bind env r p e in
    (env', ty_top t)
  | Top_expr e ->
    (env, ty_top (expression env e))
