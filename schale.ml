let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Format.print_string "@> ";
      Format.print_flush ();
      let p = Parser.top_phrase Lexer.token lexbuf in
      Format.printf "%a" Ptree.pp_top_phrase p;
      Format.print_newline ();
    done
  with Lexer.EOF ->
    print_newline (); exit 0
