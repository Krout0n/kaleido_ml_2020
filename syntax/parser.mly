%{
open Ast
%}

%token PLUS MINUS LParen RParen Def Semi
%token <int> NUMBER
%token <string> IDENT

%start toplevel
%type <toplevel> toplevel
%%

toplevel:
    | f = func Semi { Func f }
    | e = exp Semi { Expr e }
    
func:
    | Def p=proto body=exp { Function (p, body) }

proto:
    | func_name=IDENT LParen arg=IDENT* RParen { Prototype (func_name, Array.of_list arg) }

exp:
    | l = literal_expr PLUS r = literal_expr { Binary ('+', l, r) }
    | l = literal_expr { l }

literal_expr:
    | n = NUMBER { Number ( float_of_int n ) }
    | i = IDENT { Variable i }
