%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> 
#define MAX_CAPACITY 1024
int yylex();
double ans = 0;
int id_count = 0, temp_id_count = 0, temp_arr_size = 0, arr_or_not = 0;
extern int valid, pre_c, pre_l, charCount, lineCount;
char *arr_id[MAX_CAPACITY / 4];
void yyerror(const char* message) {
    printf("Invaild format\n");
}; 
typedef struct Identifier{
    char* name; // longest id is 15 characters
	char* type;
    double value;
	int isarray;
	int arraysize;
} Identifier;  
Identifier id_table[MAX_CAPACITY];
void add_id_table(char* typename)
{
	// check this ID exist in the id_table or not
	int existing = 0; // exist in id_table or not
	for(int i = 0; i < temp_id_count; i++)
	{
		int j = 0;
		existing = 0;
		for(j = 0; j < id_count; j++)
		{
			if(id_table[j].name == arr_id[i])
			{
				existing = 1;
				break;
			}
		}
		if(existing)
		{
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("ID Redeclaration: This ID '%s' is already declared.\n", arr_id[i]);
		}
		else
		{
			if(arr_or_not)// this id is array
			{
				id_table[id_count].isarray = arr_or_not;
				id_table[id_count].arraysize = temp_arr_size;
			}
			else
			{
				id_table[id_count].isarray = arr_or_not;
				id_table[id_count].arraysize = 0;
			}
			id_table[id_count].name = arr_id[i];
			for(int k = 0; k < strlen(typename); k++)
				typename[k]=tolower(typename[k]);
			id_table[id_count++].type = typename;
		}
		
	}
	arr_or_not = 0;
	temp_arr_size = 0;
	temp_id_count = 0;
}
char* find_in_id_table(char* idname)
{
	for(int i = 0; i < strlen(idname); i++) 
		idname[i] = tolower(idname[i]);
	for(int i = 0; i < id_count; i++) 
		if(strcmp(id_table[i].name, idname) == 0) 
			return id_table[i].type;
	return "";
}
char* find_exp_type(char* findexp)
{
	if(strcmp(findexp, "ntype") == 0) 
		return "ntype";
	for(int i = 0; i < strlen(findexp); i++) 
		if(findexp[i]=='.') 
			return "real";
	for(int i = 0; i < strlen(findexp); i++) 
		if(findexp[i] >= 48 && findexp[i] <= 57) // is 0~9 or not
			return "integer";
	// is ID
	for(int i = 0; i < strlen(findexp); i++) 
		findexp[i] = tolower(findexp[i]);
	for(int i = 0; i < id_count; i++) 
		if(strcmp(id_table[i].name, findexp) == 0) 
			return id_table[i].type;
	return ""; // not existed ID
}
%}
%union 
{
	char* reserved_word;
	char* symbol;
	char* name;
} 
%type <name> prog prog_name dec_list dec id_list type array_type standtype stmt_section stmt_list assign for initial_expression final_expression sim_exp term exp relational_operator var_id if body read read_list write write_list value number
%token <reserved_word> ABSOLUTE AND BBEGIN BREAK CASE CONST CONTINUE DO ELSE END FOR FUNCTION IF MOD NIL NOT OBJECT OF OR PROGRAM THEN TO VAR WHILE ARRAY INTEGER DOUBLE WRITE WRITELN STRING CHAR FLOAT READ  //reserved word
%token <symbol> ASSIGN EQUALLNESS LE GE SEMICOLON COLON COMMA NOTEQUAL SLASH UPARROW LPARENTHESES RPARENTHESES LT GT EQUAL LSB RSB PLUS MINUS MUL DIVIDE DOT NEWLINE
%token <name> ID INT REAL STR
%%
prog : PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BBEGIN stmt_section SEMICOLON END DOT {printf("\n");}
	 | PROGRAM prog_name SEMICOLON VAR dec_list SEMICOLON BBEGIN stmt_section SEMICOLON END 
	 {
		valid = 0; // error
		printf("\rLine %d, at char %d, ",pre_l,pre_c+3);
		printf("syntax error, unexpected $end, expecting DOT'.'\n");
	 };
	 //| error;
prog_name: ID;

dec_list: dec_list SEMICOLON dec 
		| dec;
dec: id_list COLON type;// record the id_name first, then check if it can added into id_table later.
id_list : ID {arr_id[temp_id_count++] = $1;} // record the declared id in var section 
	    | id_list COMMA ID {arr_id[temp_id_count++] = $3;}; 
type: array_type 
	| standtype {arr_or_not = 0; temp_arr_size = 0; add_id_table($1);}; // deal with the type of id_list and add them into id_table
array_type: ARRAY LSB INT DOT DOT INT RSB OF standtype {arr_or_not = 1; temp_arr_size = atoi($6) - atoi($3); add_id_table($9);};
standtype: INTEGER { $$ = strdup($1);}
		 | STRING { $$ = strdup($1);}
		 | DOUBLE { $$ = strdup($1);}
		 | FLOAT { $$ = strdup($1);}
		 | CHAR { $$ = strdup($1);};

stmt_section: stmt_section SEMICOLON stmt_list 
			| stmt_list;

stmt_list: assign
		 | for
		 | if
		 | read 
		 | write  
		 | WRITELN { $$ = strdup($1);};

assign: var_id ASSIGN sim_exp
	  {
		if(strcmp(find_in_id_table($1), "") == 0) // $1 not exist in id_tale
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
		else if(strcmp(find_exp_type($3), "") == 0) // $3 is an ID, but not declared yet
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$3);
		}
		else if(strcmp(find_exp_type($3), "ntype") == 0) // not real or integer or ID
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			//printf("Incompatible types: got '%s', expected '%s'                    \n",temp,temp2);
			printf("WRONG                                           \n");
		}
		else if(strcmp(find_in_id_table($1), find_exp_type($3))!=0 && strcmp(find_exp_type($3),"ntype")!=0)
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", find_exp_type($3), find_in_id_table($1));
		}
	  }
	  | var_id relational_operator sim_exp
	  {
		valid = 0; // error
		if(find_in_id_table($1) == "") // not exist in id_tale
		{
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
		else // can't use relational_operator in the middle
		{
			printf("\rLine %d, ",pre_l);
			printf("syntax error, unexpected '%s', expecting ':='\n",$2);
		}
	  }
	  | var_id ASSIGN STR
	  {
		if(find_in_id_table($1) == "") // not exist in id_tale
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
		else if(strcmp(find_in_id_table($1), "string") != 0) // mismatch type
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible type: here got '%s', expecting 'string'\n",find_in_id_table($1));
		}
	  }
	  | ;//error;
for: FOR var_id ASSIGN initial_expression TO final_expression DO body;
initial_expression: sim_exp;
final_expression: exp;

sim_exp: term
	   | sim_exp PLUS term
	   {
			char* temp_str1 = find_exp_type($3);
			char* temp_str2 = find_exp_type($1);
			if(strcmp(temp_str1, temp_str2) != 0) // type mismatch 
				$$ = strdup("ntype");
			else // same type
				$$ = $1;
			// free(temp_str1);
            // free(temp_str2);
	   }
	   | sim_exp MINUS term
	   {
			char* temp_str1 = find_exp_type($3);
			char* temp_str2 = find_exp_type($1);
			if(strcmp(temp_str1, temp_str2) != 0 && strcmp(temp_str1, "ntype") != 0) // type mismatch 
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_str1, temp_str2);
				$$ = strdup("ntype");
			}
			else if(strcmp(temp_str1, "ntype") == 0)
				$$ = strdup("ntype");
			else 
				$$ = $1;
	   }
	   | sim_exp OR term
	   {
			char* temp_str1 = find_exp_type($3);
			char* temp_str2 = find_exp_type($1);
			if(strcmp(temp_str1, temp_str2) != 0 && strcmp(temp_str1, "ntype") != 0) // type mismatch 
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_str1, temp_str2);
				$$ = strdup("ntype");
			}
			else if(strcmp(temp_str1, "ntype") == 0)
				$$ = strdup("ntype");
			else 
				$$ = $1;
	   };

term: value
	| term MUL value
	{
		char* temp_str1 = find_exp_type($3);
		char* temp_str2 = find_exp_type($1);
		if(strcmp(temp_str1, temp_str2) != 0 && strcmp(temp_str1, "ntype") != 0) // type mismatch 
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_str1, temp_str2);
			$$ = strdup("ntype");
		}
		else if(strcmp(temp_str1, "ntype") == 0)
			$$ = strdup("ntype");
		else 
			$$ = $1;
	}
	| term DIVIDE value
	{
		char* temp_str1 = find_exp_type($3);
		char* temp_str2 = find_exp_type($1);
		if(strcmp(temp_str1, temp_str2) != 0 && strcmp(temp_str1, "ntype") != 0) // type mismatch 
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_str1, temp_str2);
			$$ = strdup("ntype");
		}
		else if(strcmp(temp_str1, "ntype") == 0)
			$$ = strdup("ntype");
		else 
			$$ = $1;
		
	}
	| term MOD value
	{
		char* temp_str1 = find_exp_type($3);
		char* temp_str2 = find_exp_type($1);
		if(strcmp(temp_str1, temp_str2) != 0 && strcmp(temp_str1, "ntype") != 0) // type mismatch 
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_str1, temp_str2);
			$$ = strdup("ntype");
		}
		else if(strcmp(temp_str1, "ntype") == 0)
			$$ = strdup("ntype");
		else 
			$$ = $1;
		
	};
exp: sim_exp
   | exp relational_operator sim_exp;
relational_operator: LE | GE | LT | GT | NOTEQUAL | EQUAL;
var_id: ID
	  | ID LSB sim_exp RSB
	  {
		char *temp_str = find_exp_type($3);
		if(strcmp(temp_str, "") == 0)
		{
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("Wrong index format '%s', '%s' may be an ID. But, not found in id_table.                  \n", $3, $3);
		}
		else if(strcmp(temp_str, "integer") != 0)
		{
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("Incompatible index types: got '%s'                     ,expecting 'integer index'\n", temp_str);
		}
		
	};

if: IF LPARENTHESES exp RPARENTHESES THEN body;
  | IF LPARENTHESES exp RPARENTHESES THEN body ELSE body;
body: stmt_list
	| BBEGIN stmt_section SEMICOLON END;	
read: READ LPARENTHESES read_list RPARENTHESES;
read_list : var_id 
	{
		if(find_exp_type($1)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s'                    \n",$1);
		}
	}
	| STR 
	| read_list COMMA var_id 
	{
		if(find_exp_type($3)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s'                    \n",$3);
		}
	}
	| read_list COMMA STR;
write : WRITE LPARENTHESES write_list RPARENTHESES;
write_list : var_id 
	{
		if(find_exp_type($1)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s'                    \n",$1);
		}
	}
	| STR 
	| write_list COMMA var_id 
	{
		if(find_exp_type($3)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s'                    \n",$3);
		}
	}
	| write_list COMMA STR;

value: var_id
	 | number 
	 | STR;
	 | LPARENTHESES sim_exp RPARENTHESES;
number: INT
	  | REAL;
%%
int main()
{
	yyparse();
	return 0;
}