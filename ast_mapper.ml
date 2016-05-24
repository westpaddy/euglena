open Ast

type mapper = {
  ty : mapper -> ty -> ty;
  type_expr : mapper -> type_expr -> type_expr;
  identifier : mapper -> identifier -> identifier;
  pattern : mapper -> pattern -> pattern;
  expression : mapper -> expression -> expression;
  top_phrase : mapper -> top_phrase -> top_phrase;
}

module T = struct
  let map iter {t_desc = desc; t_level = level; t_id = id} =
    let t_desc = match desc with
      | Ty_var _ as d -> d
      | Ty_fun (t1, t2) ->
        Ty_fun (iter.ty iter t1, iter.ty iter t2)
      | Ty_const _ as d -> d
      | Ty_refine (t, p, e) ->
        Ty_refine (iter.ty iter t, iter.pattern iter p, iter.expression iter e)
      | Ty_link t ->
        Ty_link (iter.ty iter t)
      | Ty_subst t ->
        Ty_subst (iter.ty iter t)
    in
    let t_level = level in
    let t_id = id in
    {t_desc; t_level; t_id}
end

module TE = struct
  let map iter {te_desc = desc; te_ty = ty} =
    let te_desc = match desc with
      | Type_var _ as d -> d
      | Type_fun (te1, te2) ->
        Type_fun (iter.type_expr iter te1, iter.type_expr iter te2)
      | Type_const _ as d -> d
      | Type_refine (p, e) ->
        Type_refine (iter.pattern iter p, iter.expression iter e)
    in
    let te_ty = iter.ty iter ty in
    {te_desc; te_ty}
end

module I = struct
  let map iter {i_desc = desc; i_uid = uid; i_ty = ty} =
    let i_desc = desc in
    let i_uid = uid in
    let i_ty = iter.ty iter ty in
    {i_desc; i_uid; i_ty}
end

module P = struct
  let map iter {p_desc = desc; p_ty = ty} =
    let p_desc = match desc with
      | Pat_var i ->
        Pat_var (iter.identifier iter i)
      | Pat_annot (p, te) ->
        Pat_annot (iter.pattern iter p, iter.type_expr iter te)
    in
    let p_ty = iter.ty iter ty in
    {p_desc; p_ty}
end

module E = struct
  let map iter {e_desc = desc; e_ty = ty} =
    let e_desc = match desc with
      | Expr_let (r, p, e1, e2) ->
        Expr_let (r, iter.pattern iter p, iter.expression iter e1, iter.expression iter e2)
      | Expr_if (e1, e2, e3) ->
        Expr_if (iter.expression iter e1, iter.expression iter e2, iter.expression iter e3)
      | Expr_fun (p, e) ->
        Expr_fun (iter.pattern iter p, iter.expression iter e)
      | Expr_app (e1, e2) ->
        Expr_app (iter.expression iter e1, iter.expression iter e2)
      | Expr_var i ->
        Expr_var (iter.identifier iter i)
      | Expr_int _ as d -> d
      | Expr_bool _ as d -> d
      | Expr_nil as d -> d
      | Expr_cast (e, te) ->
        Expr_cast (iter.expression iter e, iter.type_expr iter te)
      | Expr_dyn (e, t) ->
        Expr_dyn (iter.expression iter e, iter.ty iter t)
    in
    let e_ty = iter.ty iter ty in
    {e_desc; e_ty}
end

module TP = struct
  let map iter {tp_desc = desc; tp_ty = ty} =
    let tp_desc = match desc with
      | Top_let (r, p, e) ->
        Top_let (r, iter.pattern iter p, iter.expression iter e)
      | Top_expr e ->
        Top_expr (iter.expression iter e)
    in
    let tp_ty = iter.ty iter ty in
    {tp_desc; tp_ty}
end

let default = {
  ty = T.map;
  type_expr = TE.map;
  identifier = I.map;
  pattern = P.map;
  expression = E.map;
  top_phrase = TP.map;
}
