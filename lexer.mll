{
  let keywords = [
    ("if", Parser.IF);
    ("then", Parser.THEN);
    ("else", Parser.ELSE);
    ("let", Parser.LET);
    ("rec", Parser.REC);
    ("in", Parser.IN);
    ("fun", Parser.FUN);
    ("true", Parser.TRUE);
    ("false", Parser.FALSE);
  ]

  exception EOF
}

rule token = parse
    [' ' '\t' '\n']+ { token lexbuf }
  | eof { raise EOF }
  | '\'' { Parser.QUOTE }
  | '(' { Parser.LPAREN }
  | ')' { Parser.RPAREN }
  | '[' { Parser.LBRACKET }
  | ']' { Parser.RBRACKET }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '*' { Parser.STAR }
  | '/' { Parser.SLASH }
  | '<' { Parser.LT }
  | '>' { Parser.GT }
  | '=' { Parser.EQ }
  | ':' { Parser.COL }
  | '@' { Parser.AT }
  | "->" { Parser.RARROW }
  | "::" { Parser.COLCOL }
  | ";;" { Parser.SEMISEMI }
  | ['0'-'9']+ as lxm { Parser.INT (int_of_string lxm) }
  | ['_' 'a'-'z'] [''' '_' 'a'-'z' 'A'-'Z' '0'-'9']* as id
    { try List.assoc id keywords with Not_found -> Parser.VAR id }
