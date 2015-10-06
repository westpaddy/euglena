open Ast

let test (e, ty) =
  try
    let p = Parser.top_phrase Lexer.token (Lexing.from_string e) in
    let ty' = begin try
        let t = snd (Typing.top_phrase Predef.env p) in
        Format.fprintf Format.str_formatter "%a" Types.pp_repr t;
        Format.flush_str_formatter ()
      with
      | Typing.Unify ->
        "Expected error"
    end
    in
    let ty = if ty = ";;" then "Expected error" else
        let p = Parser.parse_type_expr Lexer.token (Lexing.from_string ty) in
        let t = Typing.type_expr p in
        Format.fprintf Format.str_formatter "%a" Types.pp_repr t;
        Format.flush_str_formatter ()
    in
    if ty <> ty' then Format.printf "----\nterm: %s\nexpect: %s\ninferred: %s\n----\n" e ty ty'
  with
  | _ ->
    ()

let () = List.iter test Testdata.table
