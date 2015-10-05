type ty = {
  mutable t_desc : ty_desc;
  t_level : int;
}[@@deriving show]

and ty_desc =
  | Ty_var of string
  | Ty_fun of ty * ty
  | Ty_const of string
  | Ty_link of ty
[@@deriving show]

and pattern = {
  p_desc : pat_desc;
  mutable p_ty :  ty option;
}[@@deriving show]

and pat_desc =
  | Pat_var of string
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
