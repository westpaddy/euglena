let uid = ref 0

let empty = []

let extend env x t =
  incr uid;
  x.Ast.i_uid <- !uid;
  (x, t) :: env

let rec lookup env x =
  match env with
  | [] -> raise Not_found
  | (x', t) :: xs ->
    if x.Ast.i_desc = x'.Ast.i_desc then begin x.Ast.i_uid <- x'.Ast.i_uid; t end else lookup xs x
