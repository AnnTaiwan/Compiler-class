/* Definition */
%{
#include<stdio.h>
#include<string.h>
#define MAX_CAPACITY 1024

typedef struct Identifier{
    char name[16]; // longest id is 15 characters
    double value;
} Identifier;
unsigned charCount = 1, idCount=0, lineCount = 1, start_line = 0, start_charCount = 0, table_capacity = 0, invalid_Comment_or_not = 0, length_comment=0, length_invalid_Comment = 0, last_token_type = 0;
// last_token_type = {0: ID or invalid ID, 1: int or real, 3: other}
char buf[MAX_CAPACITY];
Identifier table[MAX_CAPACITY];
int lookup_table(char id_name[16]);
void save_id_into_table(char id_name[16]);
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



/* still a lot of possibilities, like how to distinguish 'ab is invalid string or ID. Can find my-ID is invalid*/
invalid_ID [a-zA-Z_][a-zA-Z0-9_]{15,}|[0-9^#]+[a-zA-Z0-9_]*|[a-zA-Z_]([a-zA-Z_]*[^a-zA-Z0-9_\n\r\n\.\,\;\:\(\)\+\-\*\/ ]+[a-zA-Z0-9_]*)+
/* match the invalid string with one single quote in front of the string*/
invalid_String ([\']([^\'\n\r\n;]|\'\'[^\'\n\r\n;]*)*)|(\'([^\'\n\r\n;]*([^\'\n\r\n;]+\'[^\'\n\r\n;]+)[^\'\n\r\n;]*)+\')

invalid_fraction \.[0-9]+[eE]|.[eE]

invalid_Hexadecimal ([\+\-]\ *[$]([0-9a-fA-F]*[g-zG-Z][0-9a-fA-F]*)+)|([$]([0-9a-fA-F]*[g-zG-Z][0-9a-fA-F]*)+)
invalid_Octal ([\+\-]\ *[&]([0-7]*[A-Za-z89][0-7]*)+)|([&]([0-7]*[A-Za-z89][0-7]*)+)

invalid_Real [\+\-]\ *[0-9]+{invalid_fraction}|[0-9]+{invalid_fraction}|[\+\-]?{invalid_fraction}|[\+\-]?{fraction}|{Integer}\.


/* find the comment */
%x COMMENT
/* find if any character left after the end of comment *) */
%x CHECK_COM 

/* Lex Rules */
%%

{Reserved} {
    last_token_type = 3;
	printf("Line: %d, 1st char: %d, \"%s\" is a \"reserved word\".\n", lineCount, charCount, yytext);
	charCount += yyleng; // predefined variable that stores the length of the current token (yytext).
}
{space} {
	charCount++;
}
{eol} {
	lineCount++; 
	charCount = 1;
}
{symbol} {
    last_token_type = 3;
	printf("Line: %d, 1st char: %d, \"%s\" is a \"symbol\".\n", lineCount, charCount, yytext);
    charCount += yyleng;
}
{Integer}|{Hexadecimal}|{Octal} {
    int temp_count = charCount, j = 0, start_idx = 0;
    memset(buf, 0, sizeof(buf));
    if(last_token_type != 3){ // exist ID number before this integer, so print the operator
        if(yytext[0] == '+' || yytext[0] == '-') // exist operator
        {
            printf("Line: %d, 1st char: %d, \"%c\" is a \"symbol\".\n", lineCount, charCount, yytext[0]);
            temp_count++;
            start_idx = 1;
            for(int i = start_idx; i < yyleng; i++){
                if(yytext[i] == ' ') // skip space
                {
                    temp_count++;
                    continue;
                }
                
                buf[j] = yytext[i];
                j++;
            }
            buf[j] = '\0';
            printf("Line: %d, 1st char: %d, \"%s\" is an \"integer\".\n", lineCount, temp_count, buf);
        }
        else{
            printf("Line: %d, 1st char: %d, \"%s\" is an \"integer\".\n", lineCount, charCount, yytext);
        }
    }
    else{ // just print the yytext
        printf("Line: %d, 1st char: %d, \"%s\" is an \"integer\".\n", lineCount, charCount, yytext);
    }
    charCount += yyleng; 
    last_token_type = 1;
}
{Real} {
    int temp_count = charCount, j = 0, start_idx = 0;
    memset(buf, 0, sizeof(buf));
    if(last_token_type != 3){ // exist ID number before this integer, so print the operator
        if(yytext[0] == '+' || yytext[0] == '-') // exist operator
        {
            printf("Line: %d, 1st char: %d, \"%c\" is a \"symbol\".\n", lineCount, charCount, yytext[0]);
            temp_count++;
            start_idx = 1;
            for(int i = start_idx; i < yyleng; i++){
                if(yytext[i] == ' ') // skip space
                {
                    temp_count++;
                    continue;
                }
                
                buf[j] = yytext[i];
                j++;
            }
            buf[j] = '\0';
            printf("Line: %d, 1st char: %d, \"%s\" is a \"real\".\n", lineCount, temp_count, buf);
        }
        else{
            printf("Line: %d, 1st char: %d, \"%s\" is a \"real\".\n", lineCount, charCount, yytext);
        }
    }
    else{ // just print the yytext
        printf("Line: %d, 1st char: %d, \"%s\" is a \"real\".\n", lineCount, charCount, yytext);
    }
    charCount += yyleng; 
    last_token_type = 1;
}
{ID} { // valid ID
    last_token_type = 0;
    printf("Line: %d, 1st char: %d, \"%s\" is an \"ID\".\n", lineCount, charCount, yytext);

    char id_name[16];
    strncpy(id_name, yytext, 16);
    id_name[15] = '\0'; // make sure end in null
    int exist_or_not = lookup_table(id_name);
    if(exist_or_not == 0) // not existed yet
    {
        save_id_into_table(id_name); // save this id into table
    }
    charCount += yyleng;
}

{String} { // valid String
    last_token_type = 3;
    if(yyleng <= 32){
		memset(buf, 0, sizeof(buf));
        int skip_idx = -1, j = 0;
        for(int i = 0; i < yyleng - 1; i++){ // delete the second single quote if appearing two sequential single quotes
            if(i == skip_idx)
                continue;
            else if(yytext[i] == '\'' && yytext[i+1] == '\''){
                skip_idx = i + 1;
            }
            buf[j] = yytext[i];
            j++;
        }
        buf[j] = yytext[yyleng - 1]; // the last single quote
        buf[j+1] = '\0'; // end of the string
        printf("Line: %d, 1st char: %d, \"%s\" is a \"string\".\n", lineCount, charCount, buf);
        
    }
    else{
        printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"string\".\n", lineCount, charCount, yytext);
        // too long
        printf("ERROR: The string \"%s\" is too long.\n", yytext);
    }
    charCount += yyleng; 
}

"(*" { // start of comment
    last_token_type = 3;
	memset(buf,0 , sizeof(buf));
    length_comment = yyleng;
	strcat(buf, yytext);
	start_line = lineCount;
    start_charCount = charCount;
    charCount += yyleng; 
	BEGIN COMMENT;
}
<COMMENT>"*)" { // end of comment, then to check the next character in that line
    invalid_Comment_or_not = 1; // invalid comment
	strcat(buf,yytext);
    length_comment += yyleng;
    length_invalid_Comment = length_comment; // may be invalid comment
    charCount += yyleng; 
	BEGIN CHECK_COM; // default initial state
}
<COMMENT>{eol} {
	lineCount++;
    charCount = 1;
    length_comment += yyleng;
	strcat(buf,yytext);	
}
<COMMENT>. {	
	strcat(buf,yytext);
    length_comment += yyleng;
    charCount+=yyleng;
}

<CHECK_COM>{eol} { // \r\n means this comment end successfully
    if(invalid_Comment_or_not)
    {
	    buf[length_comment] = '\0';
        printf("Line: %d,1st char: %d, \"%s\" is a \"comment\".\n", start_line, start_charCount, buf);
    }
    else
    {
        buf[length_invalid_Comment] = '\0';
        printf("Line: %d,1st char: %d, \"%s\" is an invalid \"comment\".\n", start_line, start_charCount, buf);
    }
    lineCount++;
    charCount = 1;
    BEGIN INITIAL; // go back to default initial state
}
<CHECK_COM>"*)" { // another end of comment
    invalid_Comment_or_not = 0; // invalid comment
	strcat(buf,yytext);
    charCount += yyleng; 
    length_invalid_Comment += yyleng;
}
<CHECK_COM>{space} { 
    strcat(buf,yytext);
    charCount++;
    length_invalid_Comment += yyleng;
}
<CHECK_COM>[^\ ] {	
    invalid_Comment_or_not = 0; // invalid comment
	strcat(buf,yytext);
    charCount+=yyleng;
    length_invalid_Comment += yyleng;
}


{invalid_ID} { // invalid ID
    last_token_type = 0;
    printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"ID\".\n", lineCount, charCount, yytext);
    if(yyleng > 15) // too long
        printf("ERROR: The ID \"%s\" is too long.\n", yytext);
    charCount += yyleng;
}
{invalid_String} {
    last_token_type = 3;
    
    printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"string\".\n", lineCount, charCount, yytext);
    charCount += yyleng; 
}
{invalid_Real} {
    int temp_count = charCount, j = 0, start_idx = 0;
    memset(buf, 0, sizeof(buf));
    if(last_token_type != 3){ // exist ID number before this integer, so print the operator
        if(yytext[0] == '+' || yytext[0] == '-') // exist operator
        {
            printf("Line: %d, 1st char: %d, \"%c\" is a \"symbol\".\n", lineCount, charCount, yytext[0]);
            temp_count++;
            start_idx = 1;
            for(int i = start_idx; i < yyleng; i++){
                if(yytext[i] == ' ') // skip space
                {
                    temp_count++;
                    continue;
                }
                
                buf[j] = yytext[i];
                j++;
            }
            buf[j] = '\0';
            printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"real\".\n", lineCount, temp_count, buf);
        }
        else{
            printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"real\".\n", lineCount, charCount, yytext);
        }
    }
    else{ // just print the yytext
        printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"real\".\n", lineCount, charCount, yytext);
    }
    charCount += yyleng;
    last_token_type = 1; // although it is invalid real, treat it like real in order to match the likely operator later

}
{invalid_Hexadecimal}|{invalid_Octal} {
    int temp_count = charCount, j = 0, start_idx = 0;
    memset(buf, 0, sizeof(buf));
    if(last_token_type != 3){ // exist ID number before this integer, so print the operator
        if(yytext[0] == '+' || yytext[0] == '-') // exist operator
        {
            printf("Line: %d, 1st char: %d, \"%c\" is a \"symbol\".\n", lineCount, charCount, yytext[0]);
            temp_count++;
            start_idx = 1;
            for(int i = start_idx; i < yyleng; i++){
                if(yytext[i] == ' ') // skip space
                {
                    temp_count++;
                    continue;
                }
                
                buf[j] = yytext[i];
                j++;
            }
            buf[j] = '\0';
            printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"integer\".\n", lineCount, temp_count, buf);
        }
        else{
            printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"integer\".\n", lineCount, charCount, yytext);
        }
    }
    else{ // just print the yytext
        printf("Line: %d, 1st char: %d, \"%s\" is an invalid \"integer\".\n", lineCount, charCount, yytext);
    }
    charCount += yyleng;
    last_token_type = 1; // although it is invalid real, treat it like real in order to match the likely operator later
}

. {printf("Line: %d,1st char: %d, \"%s\" is an \"unknown\".\n", lineCount, charCount, yytext); charCount+=yyleng; last_token_type = 3;}
    /* User code */
%%

int main()
{
	last_token_type = 3; // other
    yylex(); 	

    printf("The symbol table contains:\n");
    for(int i = 0; i < table_capacity; i++){
        printf("%s\n", table[i].name);
    }
	return 0;
}
// not save the value yet
int lookup_table(char id_name[16])
{
    if(table_capacity == 0) return 0;//table is empty, so don't exist this id.
    for(int i = 0; i < table_capacity; i++)
    {
        if(strncmp(table[i].name, id_name, 16) == 0) // the same
        {
            return 1; // id already exist in the table
        }
    }
    return 0;
}
void save_id_into_table(char id_name[16])
{
    strncpy(table[table_capacity].name, id_name, 16); // copy this new id into table
    table[table_capacity].name[15] = '\0';
    table_capacity++;
}