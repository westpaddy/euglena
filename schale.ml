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
          Format.printf "@[%a@.%a@.@]" Pprint_ast.top_phrase p Pprint_ast.ty t;
      with
      | Parser.Error ->
        Format.printf "@[Parsing error@]@."
      | Typing.Unify ->
        Format.printf "Type error@."
      end
    done
  with Lexer.EOF ->
    print_newline (); exit 0
