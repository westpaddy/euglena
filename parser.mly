%{
    open Ast

    let mk_top tp_desc = { tp_desc; tp_ty = None }
    let mk_expr e_desc = { e_desc; e_ty = None }
    let mk_pat p_desc = { p_desc; p_ty = None }
    let mk_type_expr te_desc = { te_desc; te_ty = None }
%}

%token IF THEN ELSE LET REC IN FUN TRUE FALSE
%token LPAREN RPAREN PLUS MINUS STAR SLASH LT GT EQ RARROW SEMISEMI QUOTE
%token COL
%token <int> INT
%token <string> VAR

%nonassoc IN
%right RARROW
%nonassoc ELSE
%left LT GT EQ
%left PLUS MINUS
%left STAR SLASH

%start <Ast.top_phrase> top_phrase
%start <Ast.type_expr> parse_type_expr

%%

top_phrase:
  LET; r = boption(REC); x = pat; EQ; e = expr; SEMISEMI;
    { mk_top (Top_let (r, x, e)) }
| LET; r = boption(REC); f = VAR; params = nonempty_list(pat); EQ; e = expr; SEMISEMI;
    { mk_top (Top_let (r, mk_pat (Pat_var f), List.fold_left (fun e p -> mk_expr (Expr_fun (p, e))) e (List.rev params))) }
| e = expr; SEMISEMI;
    { mk_top (Top_expr e) }
  ;;

parse_type_expr:
  t = type_expr; SEMISEMI;
    { t }
  ;;

expr:
  LET; r = boption(REC); x = pat; EQ; e1 = expr; IN; e2 = expr;
    { mk_expr (Expr_let (r, x, e1, e2)) }
| LET; r = boption(REC); f = VAR; params = nonempty_list(pat); EQ; e1 = expr; IN; e2 = expr;
    { mk_expr (Expr_let (r, mk_pat (Pat_var f), List.fold_left (fun e p -> mk_expr (Expr_fun (p, e))) e1 (List.rev params), e2)) }
| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { mk_expr (Expr_if (e1, e2, e3)) }
| FUN; x = pat; RARROW; e = expr;
    { mk_expr (Expr_fun (x, e)) }
| e1 = expr; o = bin_op; e2 = expr;
    { mk_expr (Expr_app (mk_expr (Expr_app (o, e1)), e2)) }
| e = app_expr;
    { e }
  ;;

app_expr:
  e1 = app_expr; e2 = a_expr;
    { mk_expr (Expr_app (e1, e2)) }
| e = a_expr;
    { e }
  ;;

a_expr:
  LPAREN; e = expr; RPAREN;
    { e }
| x = VAR;
    { mk_expr (Expr_var x) }
| i = INT;
    { mk_expr (Expr_int i) }
| TRUE;
    { mk_expr (Expr_bool true) }
| FALSE;
    { mk_expr (Expr_bool false) }
  ;;

%inline bin_op:
  PLUS;
    { mk_expr (Expr_var "%+") }
| MINUS;
    { mk_expr (Expr_var "%-") }
| STAR;
    { mk_expr (Expr_var "%*") }
| SLASH;
    { mk_expr (Expr_var "%/") }
| LT;
    { mk_expr (Expr_var "%<") }
| GT;
    { mk_expr (Expr_var "%>") }
| EQ;
    { mk_expr (Expr_var "%=") }
  ;;

pat:
  x = VAR;
    { mk_pat (Pat_var x) }
| LPAREN; x = VAR; COL; type_expr; RPAREN;
    { mk_pat (Pat_var x) }
  ;;

type_expr:
  t1 = type_expr; RARROW; t2 = type_expr;
    { mk_type_expr (Type_fun (t1, t2)) }
| t = a_type_expr;
    { t }
  ;;

a_type_expr:
  LPAREN; t = type_expr; RPAREN;
    { t }
| x = VAR;
    { mk_type_expr (Type_const x) }
| QUOTE; x = VAR;
    { mk_type_expr (Type_var x) }
  ;;
