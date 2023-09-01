/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>


/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex


/* Max size of string constants, including the null terminator. */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int error_flag;
int opening_cnt;
int error(char * err_msg);
int assemble(char c);


%}

/*
 * Define names for regular expressions here.
 */

/* Keywords */

CLASS (?i:class)
ELSE (?i:else)
FI (?i:fi)
IF (?i:if)
IN (?i:in)
INHERITS (?i:inherits)
LET (?i:let)
LOOP (?i:loop)
POOL (?i:pool)
THEN (?i:then)
WHILE (?i:while)
CASE (?i:case)
ESAC (?i:esac)
OF (?i:of)
NEW (?i:new)
ISVOID (?i:isvoid)
NOT (?i:not)
TRUE t(?i:rue)
FALSE f(?i:alse)

/* Special Notations */

DARROW          =>
ASSIGN          <-
LE              <=
OTHER_SN        [{};:,\.()<=\+\-~@\*/]

/* Others */

ALPHANUMERIC [a-zA-Z0-9_]

/* Does this suffice? */
WHITESPACE [ \t\r\f\v]+

%x COMMENT1 COMMENT2 STRING

%option noyywrap

%%

 /*
  *  Nested comments, use a globle variable!
  *  C does not allow nested comments. We are better than C :)
  */

<INITIAL>-- { BEGIN(COMMENT1); }
<COMMENT1>.*\n { BEGIN(INITIAL); curr_lineno++; }
<COMMENT1><<EOF>> { BEGIN(INITIAL); }
<COMMENT1>.*

<INITIAL>\(\* { BEGIN(COMMENT2); opening_cnt++; }
<INITIAL>\*\) { return error("Unmatched *)"); }
<COMMENT2>\(\* { opening_cnt++; }
<COMMENT2>\*\) {
    opening_cnt--;
    if(opening_cnt == 0) { BEGIN(INITIAL); }
}
<COMMENT2>\n { curr_lineno++; }
<COMMENT2><<EOF>> { BEGIN(INITIAL); return error("EOF in comment"); }
<COMMENT2>.

 /*
  *  The multiple-character operators, and other special notations
  */

{DARROW}		{ return (DARROW); }
{ASSIGN}        { return (ASSIGN); }
{LE}            { return (LE); }
{OTHER_SN}      { return *yytext; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS} { return CLASS; }
{ELSE} { return ELSE; }
{FI} { return FI; }
{IF} { return IF; }
{IN} { return IN; }
{INHERITS} { return INHERITS; }
{LET} { return LET; }
{LOOP} { return LOOP; }
{POOL} { return POOL; }
{THEN} { return THEN; }
{WHILE} { return WHILE; }
{CASE} { return CASE; }
{ESAC} { return ESAC; }
{OF} { return OF; }
{NEW} { return NEW; }
{ISVOID} { return ISVOID; }
{NOT} { return NOT; }
{TRUE} {
    yylval.boolean = 1;
    return BOOL_CONST;
}
{FALSE} {
    yylval.boolean = 0;
    return BOOL_CONST;
}

[0-9]+ {
    yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}


[A-Z]{ALPHANUMERIC}* {
    yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}
[a-z]{ALPHANUMERIC}* {
    yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>\" { BEGIN(STRING); string_buf_ptr = string_buf; }
<STRING>\\n {
    if (assemble('\n')) {
        return error("String constant too long");
    }
}
<STRING>\\t {
    if (assemble('\t')) {
        return error("String constant too long");
    }
}
<STRING>\\f {
    if (assemble('\f')) {
        return error("String constant too long");
    }
}
<STRING>\\b {
    if (assemble('\b')) {
        return error("String constant too long");
    }
}
<STRING>\\\0 {
    if (!error_flag) {
        error_flag = 1;
        return error("String contains null character");
    }
}
<STRING>\\(?s:.) {
    if( *(yytext + 1) == '\n') { curr_lineno++; }
    if (assemble(*(yytext + 1))) {
        return error("String constant too long");
    }
}

<STRING>\n {
    BEGIN(INITIAL);
    if (error_flag) {
        error_flag = 0;
    } else {
        return error("Unterminated string constant");
    }
}

<STRING>\" { 
    BEGIN(INITIAL);
    if (!error_flag) {
        *string_buf_ptr = '\0';
        yylval.symbol = stringtable.add_string(string_buf);
        return STR_CONST;
    } else {
        error_flag = 0;
    }
}

<STRING>\0 {
    if (!error_flag) {
        error_flag = 1;
        return error("String contains null character");
    }
}

<STRING><<EOF>> {
    BEGIN(INITIAL);
    if (!error_flag) {
        return error("EOF in string constant");
    }
}

<STRING>. {
    if (assemble(*yytext)) {
        return error("String constant too long");
    }
}

{WHITESPACE}
\n { curr_lineno++; }
. { return error(yytext); }

%%

/* User subroutines. Prototypes in Declarations */
int error(char* err_msg) {
    yylval.error_msg = err_msg;
    return ERROR;
}

int assemble(char c) {
    if (error_flag) return 0;
    if (string_buf_ptr - string_buf == MAX_STR_CONST - 1) {
        error_flag = 1;
        return 1;
    }
    *string_buf_ptr = c;
    string_buf_ptr++;
    return 0;
}
