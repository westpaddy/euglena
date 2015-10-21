open Ast

type t = identifier list

let empty = []

let extend env x = x :: env

let rec lookup env x =
  match env with
  | [] -> raise Not_found
  | x' :: xs ->
    if x.i_desc = x'.i_desc then x' else lookup xs x
