/* Definition */
%{
#include<stdio.h>
#include<string.h>
#include "y.tab.h"

// typedef struct Identifier{
//     char name[16]; // longest id is 15 characters
//     double value;
// } Identifier;
unsigned int charCount = 1, idCount=0, lineCount = 1, start_line = 0, start_charCount = 0, table_capacity = 0, invalid_Comment_or_not = 0, length_comment=0, length_invalid_Comment = 0, last_token_type = 0, valid = 1, pre_c=1, pre_l=1;

%}
/* You should write your own regular expression. */
Reserved [Aa][Bb][Ss][Oo][Ll][Uu][Tt][Ee]|[Aa][Nn][Dd]|[Bb][Ee][Gg][Ii][Nn]|[Bb][Rr][Ee][Aa][Kk]|[Cc][Aa][Ss][Ee]|[Cc][Oo][Nn][Ss][Tt]|[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee]|[Dd][Oo]|[Ee][Ll][Ss][Ee]|[Ee][Nn][Dd]|[Ff][Oo][Rr]|[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]|[Ii][Ff]|[Mm][Oo][Dd]|[Nn][Ii][Ll]|[Nn][Oo][Tt]|[Oo][Bb][Jj][Ee][Cc][Tt]|[Oo][Ff]|[Oo][Rr]|[Pp][Rr][Oo][Gg][Rr][Aa][Mm]|[Tt][Hh][Ee][Nn]|[Tt][Oo]|[Vv][Aa][Rr]|[Ww][Hh][Ii][Ll][Ee]|[Aa][Rr][Rr][Aa][Yy]|[Ii][Nn][Tt][Ee][Gg][Ee][Rr]|[Dd][Oo][Uu][Bb][Ll][Ee]|[Ww][Rr][Ii][Tt][Ee]|[Ww][Rr][Ii][Tt][Ee][Ll][Nn]|[Ss][Tt][Rr][Ii][Nn][Gg]|[Ff][Ll][Oo][Aa][Tt]|[Rr][Ee][Aa][Dd]

/* (absolute|and|begin|break|case|const|continue|do|else|end|for|function|if|mod|nil|not|object|of|or|program|then|to|var|while|array|integer|double|write|writeln|string|float|read|integer|double|write|writeln|string|float) */
Digit [0-9]
Letter [a-zA-Z]
Operator [\+\-\*]
/* put two-characters symbol first */
symbol (:=)|(==)|(<=)|(>=)|\;|\:|\(|\)|\>|\<|\=|\[|\]|\+|\-|\*|\/|\.
space [ ]
eol \r\n|\n
ID [a-zA-Z_][a-zA-Z0-9_]{0,14}
/* inside the single quote, the single quotes must be next to each other */
String [\']([^\'\n\r\n]|\'\'[^\'\n\r\n]*)*[\'] 
scale_factor [eE]([\+\-][0-9]+|[0-9]+)
Integer [\+\-]\ *[0-9]+|[0-9]+
fraction \.[0-9]+({scale_factor})?|\.{scale_factor}
Hexadecimal [\+\-]\ *[$][0-9a-fA-F]+|[$][0-9a-fA-F]+
Octal [\+\-]\ *[&][0-7]+|[&][0-7]+
/* with +/- or not, exist eE must followed number */
/* Real must have . or Ee or both */
Real [\+\-]\ *[0-9]+{fraction}|[0-9]+{fraction}|[\+\-]\ *[0-9]+({scale_factor})|[0-9]+({scale_factor})
comment [(][*]([^*]|{eol}|([*]+([^*)]|{eol})))*[*]+[)]{eol}*

%x op
/* Lex Rules */
%%
<*>[Aa][Bb][Ss][Oo][Ll][Uu][Tt][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL ;
    return ABSOLUTE;
}

<*>[Aa][Nn][Dd] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return AND;
}

<*>[Bb][Ee][Gg][Ii][Nn] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return BBEGIN;
}

<*>[Bb][Rr][Ee][Aa][Kk] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return BREAK;
}

<*>[Cc][Aa][Ss][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return CASE;
}

<*>[Cc][Oo][Nn][Ss][Tt] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return CONST;
}

<*>[Cc][Oo][Nn][Tt][Ii][Nn][Uu][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return CONTINUE;
}

<*>[Dd][Oo] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return DO;
}

<*>[Ee][Ll][Ss][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return ELSE;
}

<*>[Ee][Nn][Dd] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return END;
}

<*>[Ff][Oo][Rr] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return FOR;
}

<*>[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return FUNCTION;
}

<*>[Ii][Ff] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return IF;
}

<*>[Mm][Oo][Dd] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return MOD;
}

<*>[Nn][Ii][Ll] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return NIL;
}

<*>[Nn][Oo][Tt] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return NOT;
}

<*>[Oo][Bb][Jj][Ee][Cc][Tt]  {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return OBJECT;
}

<*>[Oo][Ff] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return OF;
}

<*>[Oo][Rr] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return OR;
}

<*>[Pp][Rr][Oo][Gg][Rr][Aa][Mm] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return PROGRAM;
}

<*>[Tt][Hh][Ee][Nn] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return THEN;
}

<*>[Tt][Oo] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return TO;
}

<*>[Vv][Aa][Rr] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return VAR;
}

<*>[Ww][Hh][Ii][Ll][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return WHILE;
}

<*>[Aa][Rr][Rr][Aa][Yy] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return ARRAY;
}

<*>[Ii][Nn][Tt][Ee][Gg][Ee][Rr] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
	BEGIN INITIAL;
    return INTEGER;
}

<*>[Dd][Oo][Uu][Bb][Ll][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return DOUBLE;
}

<*>[Ww][Rr][Ii][Tt][Ee] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return WRITE;
}

<*>[Ww][Rr][Ii][Tt][Ee][Ll][Nn] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return WRITELN;
}

<*>[Ss][Tt][Rr][Ii][Nn][Gg] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return STRING;
}

<*>[Ff][Ll][Oo][Aa][Tt] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return FLOAT;
}

<*>[Rr][Ee][Aa][Dd] {
    if(charCount == 1 && valid == 1)
        printf("Line: %d: ", lineCount);
    if(valid == 1)
        printf("%s\n", yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.reserved_word=strdup(yytext);
    BEGIN INITIAL;
	return READ;
}

<*>{space} {
	if(charCount != 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf(" ");
    charCount++;
}
<*>[ ]*{eol} {
    if(charCount != 1 && valid == 1)
        printf("\n");
    valid = 1; // reset the valid
    lineCount++;
    charCount = 1;
	return NEWLINE;
}

{symbol} {
    last_token_type = 3;
	printf("Line: %d, 1st char: %d, \"%s\" is a \"symbol\".\n", lineCount, charCount, yytext);
    charCount += yyleng;
}
<*>":=" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return ASSIGN; // assignment
}
<*>"==" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return EQUALLNESS; // test equalness
}
<*>"<=" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return LE;// less than or equal to
}
<*>">=" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return GE; // greater than or equal to 
}
<*>";" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return SEMICOLON;
}
<*>":" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return COLON;
}
<*>"," {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return COMMA;
}
<*>"<>" {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
	pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return NOTEQUAL;
}
<*>"/" {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
	pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return SLASH;
}
<*>"^" {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
	pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return UPARROW;
}
<*>"(" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return LPARENTHESES;
}
<*>")" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return RPARENTHESES;
}
<*>">" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return GT;// greater than
}
<*>"<" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return LT;// less than
}
<*>"=" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return EQUAL;
}
<*>"[" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return LSB; // left square bracket
}
<*>"]" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return RSB;// right square bracket
}
<op>"+" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return PLUS;// right square bracket
}
<op>"-" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return MINUS;// right square bracket
}
<*>"*" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return MUL;// right square bracket
}
<*>"/" {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return DIVIDE;// right square bracket
}
<*>"." {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.symbol=strdup(yytext);
    BEGIN INITIAL;
	return DOT;// right square bracket
}

<*>{ID} {
    if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.name=strdup(yytext);
    BEGIN op;
	return ID;// right square bracket
}

{Integer} {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.name=strdup(yytext);
    BEGIN op;
	return INT;// right square bracket
}
 
{Real} {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.name=strdup(yytext);
    BEGIN op;
	return REAL;// right square bracket
} 

{String} {
	if(charCount == 1 && valid == 1)
        printf("Line %d: ",lineCount);
    if(valid == 1)
        printf("%s",yytext);
    pre_c = charCount;
    pre_l = lineCount;
    charCount += yyleng;
	yylval.name=strdup(yytext);
    BEGIN INITIAL;
	return STR;// right square bracket
}	


<*>[ ]*{comment} {
	for(int i = 0; i < yyleng; i++) 
	{
		if (yytext[i]==10) // \n change line
		{
			lineCount++;
			charCount=1;
		} 
		else 
			charCount++;
	}
	BEGIN INITIAL;
}	//comment

%%
int yywrap(){
    return 1;
}
