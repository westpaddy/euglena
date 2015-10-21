%{
    open Ast

    let null_ty () = Types.new_ty (Ty_var "_")

    let mk_top tp_desc = { tp_desc; tp_ty = null_ty () }
    let mk_expr e_desc = { e_desc; e_ty = null_ty () }
    let mk_pat p_desc = { p_desc; p_ty = null_ty () }
    let mk_id s = { i_desc = s; i_uid = -1; i_ty = null_ty () }
    let mk_type_expr te_desc = { te_desc; te_ty = null_ty () }

    let mfun params body =
      List.fold_left (fun e p -> mk_expr (Expr_fun (p, e))) body (List.rev params)
%}

%token IF THEN ELSE LET REC IN FUN TRUE FALSE
%token LPAREN RPAREN PLUS MINUS STAR SLASH LT GT EQ RARROW SEMISEMI QUOTE LBRACKET RBRACKET
%token COL AT
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
    { mk_top (Top_let (r, mk_pat (Pat_var (mk_id f)), mfun params e)) }
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
    { mk_expr (Expr_let (r, mk_pat (Pat_var (mk_id f)), mfun params e1, e2)) }
| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
    { mk_expr (Expr_if (e1, e2, e3)) }
| FUN; params = nonempty_list(pat); RARROW; e = expr;
    { mfun params e }
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
    { mk_expr (Expr_var (mk_id x)) }
| i = INT;
    { mk_expr (Expr_int i) }
| TRUE;
    { mk_expr (Expr_bool true) }
| FALSE;
    { mk_expr (Expr_bool false) }
  ;;

%inline bin_op:
  PLUS;
    { mk_expr (Expr_var (mk_id "%+")) }
| MINUS;
    { mk_expr (Expr_var (mk_id "%-")) }
| STAR;
    { mk_expr (Expr_var (mk_id "%*")) }
| SLASH;
    { mk_expr (Expr_var (mk_id "%/")) }
| LT;
    { mk_expr (Expr_var (mk_id "%<")) }
| GT;
    { mk_expr (Expr_var (mk_id "%>")) }
| EQ;
    { mk_expr (Expr_var (mk_id "%=")) }
  ;;

pat:
  x = VAR;
    { mk_pat (Pat_var (mk_id x)) }
| LPAREN; p = pat; COL; t = type_expr; RPAREN;
    { mk_pat (Pat_annot (p, t)) }
| LPAREN; x = VAR; AT; e = expr; RPAREN;
    { mk_pat (Pat_annot (mk_pat (Pat_var (mk_id x)), mk_type_expr (Type_refine (mk_pat (Pat_var (mk_id x)), e)))) }
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
