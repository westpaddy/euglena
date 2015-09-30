%token IF THEN ELSE LET IN FUN TRUE FALSE
%token LPAREN RPAREN PLUS MINUS LT EQ RARROW SEMI
%token <int> INT
%token <string> VAR

%nonassoc IN
%nonassoc RARROW
%nonassoc ELSE
%left LT EQ
%left PLUS MINUS

%start <Ptree.top_phrase> top_phrase

%%

top_phrase:
  LET; x = VAR; EQ; e = expr; SEMI;
    { Ptree.T_let (x, e) }
| e = expr; SEMI;
    { Ptree.T_expr e }
  ;;

expr:
  LET; x = VAR; EQ; e1 = expr; IN; e2 = expr;
    { Ptree.E_let (x, e1, e2) }
| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { Ptree.E_if (e1, e2, e3) }
| FUN; x = VAR; RARROW; e = expr;
    { Ptree.E_fun (x, e) }
| e1 = expr; o = bin_op; e2 = expr;
    { Ptree.E_bin (o, e1, e2) }
| e = app_expr;
    { e }
  ;;

app_expr:
  e1 = app_expr; e2 = a_expr;
    { Ptree.E_app (e1, e2) }
| e = a_expr;
    { e }
  ;;

a_expr:
  LPAREN; e = expr; RPAREN;
    { e }
| x = VAR;
    { Ptree.E_var x }
| i = INT;
    { Ptree.E_int i }
| TRUE;
    { Ptree.E_bool true }
| FALSE;
    { Ptree.E_bool false }
  ;;

%inline bin_op:
  PLUS;
    { Ptree.O_plus }
| MINUS;
    { Ptree.O_minus }
| LT;
    { Ptree.O_lt }
| EQ;
    { Ptree.O_eq }
  ;;
