let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let env = ref Predef.env in
    while true do
      Format.print_string "@> ";
      Format.print_flush ();
      begin try
          let p = Parser.top_phrase Lexer.token lexbuf in
          let (new_env, t) = Typing.top_phrase !env p in
          env := new_env;
          Format.printf "%a\n%a\n" Ast.pp_top_phrase p Types.pp_repr t;
          Format.print_newline ()
        with
        | Typing.Unify ->
          Format.printf "Type error\n";
          Format.print_newline ()
      end
    done
  with Lexer.EOF ->
    print_newline (); exit 0
