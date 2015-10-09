type ty = {
  mutable t_desc : ty_desc;
  mutable t_level : int;
  t_id : int;
}[@@deriving show]

and ty_desc =
  | Ty_var of string
  | Ty_fun of ty * ty
  | Ty_const of string
  | Ty_link of ty
  | Ty_subst of ty
[@@deriving show]

and type_expr = {
  te_desc : type_expr_desc;
  mutable te_ty : ty option;
}[@@deriving show]

and type_expr_desc =
  | Type_var of string
  | Type_fun of type_expr * type_expr
  | Type_const of string
[@@deriving show]

and pattern = {
  p_desc : pat_desc;
  mutable p_ty :  ty option;
}[@@deriving show]

and pat_desc =
  | Pat_var of string
  | Pat_annot of pattern * type_expr
[@@deriving show]

and expression = {
  e_desc : expr_desc;
  mutable e_ty : ty option;
}[@@deriving show]

and expr_desc =
  | Expr_let of bool * pattern * expression * expression
  | Expr_if of expression * expression * expression
  | Expr_fun of pattern * expression
  | Expr_app of expression * expression
  | Expr_var of string
  | Expr_int of int
  | Expr_bool of bool
[@@deriving show]

and top_phrase = {
  tp_desc : top_desc;
  mutable tp_ty : ty option;
}[@@deriving show]

and top_desc =
  | Top_let of bool * pattern * expression
  | Top_expr of expression
[@@deriving show]
