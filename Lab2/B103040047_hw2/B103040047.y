%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> 
#define MAX_CAPACITY 1024
int yylex();
double ans = 0;
int id_count = 0, temp_id_count = 0, temp_arr_size = 0, arr_or_not = 0;
/*
	id_count: 		the current numger of ids in id_table
	temp_id_count: 	the current numger of ids in arr_id
	temp_arr_size: 	record the array size in declare section
	arr_or_not: 	in declare section record the id to see if it is array
*/

extern int valid, pre_c, pre_l, charCount, lineCount;

char *arr_id[MAX_CAPACITY / 4]; // record the id name in declare section, for later use to add into id_table

void yyerror(const char* message) {
	valid = 0;
	extern int yylineno;  // defined and maintained by flex
	extern char* yytext;  // defined and maintained by flex
	printf("Error: Invalid format at line %d, near '%s'.\n", yylineno, yytext);
}; 

typedef struct Identifier{
    char* name; // longest id is 15 characters
	char* type; // string real integer 
    double value;
	int isarray; // 1  : isarray
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
				typename[k]=tolower(typename[k]); // due to case-insensitive
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
		idname[i] = tolower(idname[i]); // due to case-insensitive
	for(int i = 0; i < id_count; i++) 
		if(strcmp(id_table[i].name, idname) == 0) 
			return id_table[i].type;
	return ""; // idname does not exist in ID_table
}
char* find_exp_type(char* findexp)
{
	if(strcmp(findexp, "compound_type") == 0) 
		return "compound_type";
	for(int i = 0; i < strlen(findexp); i++) 
		if(findexp[i]=='.') // exist period
			return "real";
	for(int i = 0; i < strlen(findexp); i++) 
		if(findexp[i] >= 48 && findexp[i] <= 57) // digit is 0~9 
			return "integer";
	if(findexp[0] == '\'') // is string
			return "string";
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
	char* token;
} 
%type <token> prog prog_name dec_list dec id_list type array_type standtype stmt_section stmt_list assign for initial_expression final_expression sim_exp term exp relational_operator var_id if body read read_list write write_list value number
%token <token> ABSOLUTE AND BBEGIN BREAK CASE CONST CONTINUE DO ELSE END FOR FUNCTION IF MOD NIL NOT OBJECT OF OR PROGRAM THEN TO VAR WHILE ARRAY INTEGER DOUBLE WRITE WRITELN STRING CHAR FLOAT READ RREAL  //reserved word
%token <token> ASSIGN EQUALLNESS LE GE SEMICOLON COLON COMMA NOTEQUAL UPARROW LPARENTHESES RPARENTHESES LT GT EQUAL LSB RSB PLUS MINUS MUL DIVIDE DOT NEWLINE
%token <token> ID INT REAL STR

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
dec: id_list COLON type // record the id_name first, then check if it can added into id_table later.
   | id_list ASSIGN type
   {
   		valid = 0; // error
		printf("\rLine %d, ",pre_l);
		printf("Unexpected '%s', expecting ':' for declaring the variables.               \n",$2);
   }
   | id_list relational_operator type
   {
   		valid = 0; // error
		printf("\rLine %d, ",pre_l);
		printf("Unexpected '%s', expecting ':' for declaring the variables.               \n",$2);
   };
id_list : ID {arr_id[temp_id_count++] = $1;} // record the declared id in var section 
	    | id_list COMMA ID {arr_id[temp_id_count++] = $3;}; 
type: array_type 
	| standtype {arr_or_not = 0; temp_arr_size = 0; add_id_table($1);}; // deal with the type of id_list and add them into id_table
array_type: ARRAY LSB INT DOT DOT INT RSB OF standtype {arr_or_not = 1; temp_arr_size = atoi($6) - atoi($3); add_id_table($9);};
standtype: INTEGER
		 | STRING 
		 | RREAL
		 | DOUBLE 
		 | FLOAT 
		 | CHAR;

stmt_section: stmt_section SEMICOLON stmt_list 
			| stmt_list;

			       
         
stmt_list: if
         | assign
         | for
         | read
         | write
         ;


assign: var_id ASSIGN sim_exp
	  {
		char* temp_exptype1 = find_exp_type($1);
		char* temp_exptype3 = find_exp_type($3);
		
		char* temp_idtype1 = find_in_id_table($1);
		
		//printf("	### Test: %s, %s and %s\n", temp_idtype1, temp_exptype1, temp_exptype3);
		if(strcmp(temp_idtype1, "") == 0) // $1 not existed in id_tale
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
		else if(strcmp(temp_exptype3, "") == 0) // $3 is an ID, but not declared yet
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$3);
		}
		else if(strcmp(temp_exptype3, "compound_type") == 0) // error sim_exp
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Error: got the syntax error of assignment's sim_exp part, so you can see the above message to see he details.   \n");
		}
		else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0) // acceptable, don't print error msg.
		{
			valid = 1;
		}
		else if(strcmp(temp_idtype1, temp_exptype3) != 0) // type mismatch between ID and sim_exp
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_exptype3, temp_idtype1);
		}
		
	  }
	  | var_id relational_operator sim_exp
	  {
		char* temp_exptype1 = find_exp_type($1);
		char* temp_exptype3 = find_exp_type($3);
		
		char* temp_idtype1 = find_in_id_table($1);
		
		valid = 0; // error
		if(temp_idtype1 == "") // not exist in id_tale
		{
			printf("\rLine %d, ",pre_l);
			printf("(1) identifier not found '%s', and make sure to declare it first. (2) syntax error, unexpected '%s', expecting ':='\n", $1, $2);
		}
		else if(temp_exptype3 == "") // not exist in id_tale
		{
			printf("\rLine %d, ",pre_l);
			printf("(1) identifier not found '%s', and make sure to declare it first. (2) syntax error, unexpected '%s', expecting ':='\n", $3, $2);
		}
		else if(temp_exptype3 == "compound_type") // not exist in id_tale
		{
			printf("\rLine %d, ",pre_l);
			printf("(1) Error: got the syntax error of assignment's sim_exp part, so you can see the above message to see he details. (2) syntax error, unexpected '%s', expecting ':='\n", $2);
		}
		else // can't use relational_operator in the middle
		{
			printf("\rLine %d, ",pre_l);
			printf("syntax error, unexpected '%s', expecting ':='\n",$2);
		}
	  }
	  ;
	  //| error;
for: FOR var_id ASSIGN initial_expression TO final_expression DO body;
initial_expression: sim_exp;
final_expression: exp;

sim_exp: term
	   | sim_exp PLUS term
	   {
			char* temp_exptype1 = find_exp_type($1);
			char* temp_exptype3 = find_exp_type($3);
			if(strcmp(temp_exptype3, "compound_type") == 0) // $3 is already an error
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else if(strcmp(temp_exptype1, "compound_type") == 0) // $1 is already an error
			{
				$$ = strdup("compound_type"); // still compound_type , let the higher level  deal with 
			}
			else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
			{
				$$ = $1;
			}
			else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
			{
				$$ = $3;
			}
			else if(strcmp(temp_exptype1, temp_exptype3) != 0) // simply type mismatch 
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else // same type, OK
				$$ = $1;
	   }
	   | sim_exp MINUS term
	   {
			char* temp_exptype1 = find_exp_type($1);
			char* temp_exptype3 = find_exp_type($3);
			if(strcmp(temp_exptype3, "compound_type") == 0) // $3 is already an error
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else if(strcmp(temp_exptype1, "compound_type") == 0) // $1 is already an error
			{
				$$ = strdup("compound_type"); // still compound_type , let the higher level  deal with 
			}
			else if(strcmp(temp_exptype3, "string") == 0)// can't use MINUS with string
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Error: Operator is not overloaded: '%s' - '%s'.                    \n", temp_exptype1, temp_exptype3);
				$$ = strdup("compound_type"); // still compound_type
			}
			else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
			{
				$$ = $1;
			}
			else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
			{
				$$ = $3;
			}
			else if(strcmp(temp_exptype1, temp_exptype3) != 0) // simply type mismatch 
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else // same type, OK
				$$ = $1;
	   }
	   | sim_exp OR term
	   {
			char* temp_exptype1 = find_exp_type($1);
			char* temp_exptype3 = find_exp_type($3);
			if(strcmp(temp_exptype3, "compound_type") == 0) // $3 is already an error
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else if(strcmp(temp_exptype1, "compound_type") == 0) // $1 is already an error
			{
				$$ = strdup("compound_type"); // still compound_type , let the higher level  deal with 
			}
			else if(strcmp(temp_exptype3, "string") == 0)// can't use OR with string
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Error: Operator is not overloaded: '%s' - '%s'.                    \n", temp_exptype1, temp_exptype3);
				$$ = strdup("compound_type"); // still compound_type
			}
			else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
			{
				$$ = $1;
			}
			else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
			{
				$$ = $3;
			}
			else if(strcmp(temp_exptype1, temp_exptype3) != 0) // simply type mismatch 
			{
				valid = 0; // error
				printf("\rLine %d, ",pre_l);
				printf("Incompatible types: already got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
				$$ = strdup("compound_type"); // still compound_type
			}
			else // same type, OK
				$$ = $1;
	   };

term: value
	| term MUL value
	{
		char* temp_exptype1 = find_exp_type($1);
		char* temp_exptype3 = find_exp_type($3);
		if(strcmp(temp_exptype3, "string") == 0) // can't use MUL with string
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Error: Operator is not overloaded: '%s' - '%s'.                    \n", temp_exptype1, temp_exptype3);
			$$ = strdup("compound_type"); // still compound_type
		}	
		else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
		{
			$$ = $1;
		}
		else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
		{
			$$ = $3;
		}	
		else if(strcmp(temp_exptype1, temp_exptype3) != 0) 
		{ // type mismatch and the third one is not compound type like 'integer / string'
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
			$$ = strdup("compound_type");
		}
		else 
			$$ = $1;
	}
	| term DIVIDE value
	{
		char* temp_exptype1 = find_exp_type($1);
		char* temp_exptype3 = find_exp_type($3);
		if(strcmp(temp_exptype3, "string") == 0) // can't use DIVIDE with string
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Error: Operator is not overloaded: '%s' - '%s'.                    \n", temp_exptype1, temp_exptype3);
			$$ = strdup("compound_type"); // still compound_type
		}	
		else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
		{
			$$ = $1;
		}
		else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
		{
			$$ = $3;
		}	
		else if(strcmp(temp_exptype1, temp_exptype3) != 0) 
		{ // type mismatch and the third one is not compound type like 'integer / string'
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
			$$ = strdup("compound_type");
		}
		else 
			$$ = $1;
		
	}
	| term MOD value
	{
		char* temp_exptype1 = find_exp_type($1);
		char* temp_exptype3 = find_exp_type($3);
		if(strcmp(temp_exptype3, "string") == 0) // can't use MOD with string
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("Error: Operator is not overloaded: '%s' - '%s'.                    \n", temp_exptype1, temp_exptype3);
			$$ = strdup("compound_type"); // still compound_type
		}		
		else if(strcmp(temp_exptype3, "integer") == 0 && strcmp(temp_exptype1, "real") == 0)
		{
			$$ = $1;
		}
		else if(strcmp(temp_exptype3, "real") == 0 && strcmp(temp_exptype1, "integer") == 0)
		{
			$$ = $3;
		}
		else if(strcmp(temp_exptype1, temp_exptype3) != 0) 
		{ // type mismatch and the third one is not compound type like 'integer / string'
		 // if so return this is compound type
			valid = 0; // error
			printf("\rLine %d, ", pre_l);
			printf("Incompatible types: got '%s', expecting '%s'                    \n", temp_exptype3, temp_exptype1);
			$$ = strdup("compound_type");
		}
		else // return this normal type
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
 
body: stmt_list
	| BBEGIN stmt_section SEMICOLON END;
	
	
read: READ LPARENTHESES read_list RPARENTHESES;
read_list : var_id 
	{
		if(find_exp_type($1)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
	}
	| read_list COMMA var_id 
	{
		if(find_exp_type($3)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$3);
		}
	};
write : WRITE LPARENTHESES write_list RPARENTHESES
	  | WRITELN LPARENTHESES write_list RPARENTHESES
	  | WRITE
	  | WRITELN;
write_list : var_id 
	{
		if(find_exp_type($1)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$1);
		}
	}
	| STR 
	| write_list COMMA var_id 
	{
		if(find_exp_type($3)=="")
		{
			valid = 0; // error
			printf("\rLine %d, ",pre_l);
			printf("identifier not found '%s', and make sure to declare it first.               \n",$3);
		}
	}
	| write_list COMMA STR;

value: var_id
	 | number 
	 | STR 
	 | LPARENTHESES sim_exp RPARENTHESES;
number: INT
	  | REAL
	  ;
%%
int main()
{
	yyparse();
	return 0;
}