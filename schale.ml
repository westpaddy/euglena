let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let env = ref Predef.env in
    while true do
      Format.print_string "@> ";
      Format.print_flush ();
      let p = Parser.top_phrase Lexer.token lexbuf in
      let (new_env, t) = Typing.top_phrase !env p in
      env := new_env;
      Format.printf "%a\n" Ast.pp_top_phrase p;
      Format.print_newline ();
    done
  with Lexer.EOF ->
    print_newline (); exit 0
