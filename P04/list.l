%{
    #include "list.tab.h"
    #include <cstdlib>
%}

%option noyywrap

%%

\[          return OPEN_BRACE;
\]          return CLOSE_BRACE;
,           return COMMA;
-?[0-9]+    { yylval.ll_val = std::strtoll(yytext, NULL, 10); return NUMBER; }
[ \t]+      ;
\n          return NEWLINE;
.           return *yytext;

%%
