open Ast

let test (e, ty) =
  let flag = ref true in
  let last_ty = ref None in
  let lexbuf = Lexing.from_string e in
  let env = ref Predef.env in
  begin try while true do
      let tp = Parser.top_phrase Lexer.token lexbuf in
      let new_env, tp' = Typing.top_phrase !env tp in
      last_ty := Some tp'.tp_ty;
      env := new_env
    done
  with
  | Typing.Not_bound _ | Typing.Unify _ | Lexer.EOF -> ()
  | Parser.Error -> Format.printf "@.parse error: %s" e; flag := false
  | Failure s -> Format.printf "@.%s; %s" s e; flag := false
  end;
  (* Retrieve an inferred type *)
  let ty' = match !last_ty with
    | None -> "Expected error"
    | Some ty ->
      Format.fprintf Format.str_formatter "%a" Pprint_ast.ty ty;
      Format.flush_str_formatter ()
  in
  (* Parse the expected type *)
  let ty = if ty = ";;" then "Expected error" else
      try
        let p = Parser.parse_type_expr Lexer.token (Lexing.from_string ty) in
        let t = Typing.type_expr Predef.env p in
        Format.fprintf Format.str_formatter "%a" Pprint_ast.ty t.te_ty;
        Format.flush_str_formatter ()
      with
      | _ -> Format.printf "@.parse error: %s" ty; flag := false; ""
  in
  if ty <> ty' && !flag then Format.printf "\n----\nterm: %s\nexpect: %s\ninferred: %s\n----\n" e ty ty'

let () = List.iter test Testdata.table
