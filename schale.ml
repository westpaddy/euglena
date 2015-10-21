let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let env = ref Predef.env in
    while true do
      Format.print_string "@> ";
      Format.print_flush ();
      begin try
          let tp = Parser.top_phrase Lexer.token lexbuf in
          let (new_env, tp') = Typing.top_phrase !env tp in
          env := new_env;
          let tp'' = Cast_remover.remove_cast tp' in
          Format.printf "@[%a@.@.%a@.@.@]" Pprint_ast.top_phrase tp'' Pprint_ast.ty tp''.Ast.tp_ty;
      with
      | Parser.Error ->
        Format.printf "@[Parsing error@]@."
      | Typing.Unify _ ->
        Format.printf "Type error@."
      end
    done
  with Lexer.EOF ->
    print_newline (); exit 0
