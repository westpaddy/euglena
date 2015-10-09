open Ast

let test (e, ty) =
  let flag = ref true in
  let last_ty = ref None in
  let lexbuf = Lexing.from_string e in
  let env = ref Predef.env in
  begin try while true do
        let p = Parser.top_phrase Lexer.token lexbuf in
        last_ty := Some (snd (Typing.top_phrase !env p));
      done
    with
    | Typing.Unify | Lexer.EOF -> ()
    | _ -> Format.printf "."; flag := false
  end;
  let ty' = match !last_ty with
    | None -> "Expected error"
    | Some ty ->
      Format.fprintf Format.str_formatter "%a" Types.pp_repr ty;
      Format.flush_str_formatter ()
  in
  let ty = if ty = ";;" then "Expected error" else
      try
        let p = Parser.parse_type_expr Lexer.token (Lexing.from_string ty) in
        let t = Typing.type_expr p in
        Format.fprintf Format.str_formatter "%a" Types.pp_repr t;
        Format.flush_str_formatter ()
      with
      | _ ->
        Format.printf ","; flag := false;
        "Expected error"
  in
  if ty <> ty' && !flag then Format.printf "\n----\nterm: %s\nexpect: %s\ninferred: %s\n----\n" e ty ty'

let () = List.iter test Testdata.table
