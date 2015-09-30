type bin_op =
  | O_plus
  | O_minus
  | O_lt
  | O_eq
[@@deriving show]

type expr =
  | E_let of string * expr * expr
  | E_if of expr * expr * expr
  | E_fun of string * expr
  | E_bin of bin_op * expr * expr
  | E_app of expr * expr
  | E_var of string
  | E_int of int
  | E_bool of bool
[@@deriving show]

type top_phrase =
  | T_let of string * expr
  | T_expr of expr
[@@deriving show]
