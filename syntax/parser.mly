%{
open Ast
%}

%token PLUS MINUS SEMISEMI
%token <int> NUMBER

%start toplevel
%type <expr> toplevel
%%

toplevel:
    e = exp SEMISEMI { e }

exp:
    l = literal_exp PLUS r = literal_exp { Binary ('+', l, r) }

literal_exp:
    n = NUMBER { Number ( float_of_int n ) }
