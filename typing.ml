open Ast

exception Unify of (ty * ty) list
exception Not_bound of identifier

let uid : unit -> int =
  let uid = ref 0 in
  fun () -> incr uid; !uid

let rec occur (t1 : Ast.ty) (t2 : Ast.ty) : unit =
  if t1 == t2 then raise (Unify [t1, t2]) else
    Types.iter (occur t1) t2

let rec unify (t1 : Ast.ty) (t2 : Ast.ty) : unit =
  let t1 = Types.repr t1 in
  let t2 = Types.repr t2 in
  if t1 == t2 then () else
    let d1 = t1.t_desc in
    let d2 = t2.t_desc in
    match d1, d2 with
    | Ty_var _, _ ->
      occur t1 t2;
      Types.update_level t2 t1.t_level;
      Types.link t1 t2
    | _, Ty_var _ ->
      occur t2 t1;
      Types.update_level t1 t2.t_level;
      Types.link t2 t1
    | _ ->
      occur t1 t2;
      Types.link t1 t2;
      try
        begin match d1, d2 with
          | Ty_fun (t1, t2), Ty_fun (s1, s2) ->
            unify t1 s1; unify t2 s2
          | Ty_const c1, Ty_const c2 when c1 = c2 ->
            ()
          | Ty_refine (t1, p1, e1), Ty_refine (t2, p2, e2) ->
            unify t1 t2
          | _ ->
            raise (Unify [t1, t2])
        end
      with Unify r ->
        t1.t_desc <- d1;
        raise (Unify ((t1, t2) :: r))

let rec type_expr (env : Env.t) (te : Ast.type_expr) : Ast.type_expr =
  let table = ref [] in
  let rec iter te =
    let te_desc, te_ty = match te.te_desc with
      | Type_var x ->
        let ty = begin try List.assoc x !table with Not_found ->
          let ty = Types.new_ty (Ty_var x) in
          table := (x, ty) :: !table; ty
        end
        in
        (Type_var x, ty)
      | Type_fun (te1, te2) ->
        let te1' = iter te1 in
        let te2' = iter te2 in
        (Type_fun (te1', te2'), Types.new_ty (Ty_fun (te1'.te_ty, te2'.te_ty)))
      | Type_const x ->
        (Type_const x, Types.new_ty (Ty_const x)) (* incorrect *)
      | Type_refine (p, e) ->
        let p', new_env = pattern env p in
        let e' = expression new_env e in
        unify (Types.unref e'.e_ty) Predef.ty_bool;
        (Type_refine (p', e'), Types.new_ty (Ty_refine (p'.p_ty, p', e')))
    in
    {te_desc; te_ty}
  in
  iter te

and pattern (env : Env.t) (pat : Ast.pattern) : Ast.pattern * Env.t =
  let ref_env = ref env in
  let rec iter p =
    let p_desc, p_ty = match p.p_desc with
      | Pat_var x ->
        let x' = {i_desc = x.i_desc; i_uid = uid (); i_ty = Types.fresh_var ()} in
        ref_env := Env.extend !ref_env x';
        (Pat_var x', x'.i_ty)
      | Pat_annot (p, te) ->
        let p' = iter p in
        let te' = type_expr env te in
        unify p'.p_ty te'.te_ty;
        (Pat_annot (p', te'), te'.te_ty)
    in
    {p_desc; p_ty}
  in
  let pat' = iter pat in
  (pat', !ref_env)

and expression (env : Env.t) (expr : Ast.expression) : Ast.expression =
  let e_desc, e_ty = match expr.e_desc with
    | Expr_let (r, p, e1, e2) ->
      let new_env, p', e1' = let_bind env r p e1 in
      let e2' = expression new_env e2 in
      (Expr_let (r, p', e1', e2'), e2'.e_ty)
    | Expr_if (e1, e2, e3) ->
      let e1' = expression env e1 in
      let e2' = expression env e2 in
      let e3' = expression env e3 in
      unify (Types.unref e1'.e_ty) Predef.ty_bool;
      let ret_t = Types.unref e3'.e_ty in
      unify (Types.unref e2'.e_ty) ret_t;
      (Expr_if (e1', e2', e3'), ret_t)
    | Expr_fun (p, e) ->
      let p', new_env = pattern env p in
      let e' = expression new_env e in
      (Expr_fun (p', e'), Types.new_ty (Ty_fun (p'.p_ty, e'.e_ty)))
    | Expr_app (e1, e2) ->
      let e1' = expression env e1 in
      let e2' = expression env e2 in
      unify (Types.unref e1'.e_ty) (Types.new_ty (Ty_fun (Types.unref e2'.e_ty, Types.fresh_var ())));
      let dom_t, ret_t = match (Types.repr e1'.e_ty).t_desc with
        | Ty_fun (t1, t2) ->
          (t1, t2)
        | _ -> assert false
      in
      let e2'' = {e_desc = Expr_dyn (e2', dom_t); e_ty = dom_t} in
      (Expr_app (e1', e2''), ret_t)
    | Expr_var x ->
      let x' = try Env.lookup env x with Not_found -> raise (Not_bound x) in
      (Expr_var x', Types.instantiate x'.i_ty)
    | Expr_int _ as d ->
      (d, Predef.ty_int)
    | Expr_bool _ as d ->
      (d, Predef.ty_bool)
    | Expr_cast (e, te) ->
      let e' = expression env e in
      let te' = type_expr env te in
      unify (Types.unref e'.e_ty) (Types.unref te'.te_ty);
      (Expr_dyn (e', te'.te_ty), te'.te_ty)
    | Expr_dyn _ -> assert false
  in
  {e_desc; e_ty}

and let_bind (env : Env.t) (is_rec : bool) (pat : Ast.pattern) (expr : Ast.expression)
  : Env.t * Ast.pattern * Ast.expression
  =
  Types.incr_level ();
  let pat', new_env = pattern env pat in
  let expr' = if is_rec then expression new_env expr else expression env expr in
  unify pat'.p_ty expr'.e_ty;
  Types.decr_level ();
  Types.generalize pat'.p_ty;
  (new_env, pat', expr')

let top_phrase (env : Env.t) (top : Ast.top_phrase) : Env.t * Ast.top_phrase =
  let new_env, tp_desc, tp_ty = match top.tp_desc with
    | Top_let (r, p, e) ->
      let new_env, p', e' = let_bind env r p e in
      (new_env, Top_let (r, p', e'), p'.p_ty)
    | Top_expr e ->
      let e' = expression env e in
      (env, Top_expr e', e'.e_ty)
  in
  (new_env, {tp_desc; tp_ty})
