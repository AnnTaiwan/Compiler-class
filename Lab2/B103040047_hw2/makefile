FILE_lex=	B103040047.l
FILE_YACC=	B103040047.y
all:	clean y.tab.c lex.yy.c
	gcc lex.yy.c y.tab.c  -lfl -o demo

y.tab.c:
	bison -y -d $(FILE_YACC)

lex.yy.c:
	flex $(FILE_lex)

clean:
	rm -f demo lex.yy.c y.tab.c y.tab.h
