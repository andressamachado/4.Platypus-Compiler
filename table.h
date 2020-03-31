/****************************************************************************************************
Filename:				table.h
Compiler:				Microsoft Visual Studio Enterprise 2019 Version 16.2.4
Author:					Andressa Pessoa de Araujo Machado [040923007] & Raphael Guerra [040908555]
Course:					CST8152 - Compilers
Lab Section Number:		012 &  011
Assignment Number:		2 - The Scanner
Submission Date:		23.March.2020
Professor's Name:		Svillen Ranev

Purpose:				Header file containing macros representing the states, a matriz of states for each 
						regular expression recognition step, an array containing the state 0 to state 12, 
						an array containing a related state function, and an array containg the group of 
						Platypus Language keywords. Also include the declaration of functions used in 
						the Scanner file.
****************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */

  /*  Special case tokens processed separately one by one
   *  in the token-driven part of the scanner
   *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
   *  white space
   *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', # ,
   *  .AND., .OR. , SEOF,
   */

#define SEOF 255 /* end of file */
#define SEOF_2 EOF /* -1 */
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

   /* State transition table definition */
#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*         [a-zA-Z]   0  [1-9]   .     #    other    "    SEOF*/
	/* State 0 */  { 1,   6,   4,   ES,   ES,    ES,     9,    ES},		/* Initial state */
	/* State 1 */  { 1,   1,   1,    2,   3,      2,	 2,     2},		/* Letter state */
	/* State 2 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* VID & AVID/KW accepting state with retraction */
	/* State 3 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* VID & SVID accepting state */
	/* State 4 */  { ES,  4,   4,    7,    5,     5,     5,     5},		/* NzD state */
	/* State 5 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* IL & DIL accepting state with retraction */
	/* State 6 */  { ES,  6,  ES,    7,   5,     5,     5,     5},		/* 0 digit state */
	/* State 7 */  {  8,  7,   7,    8,    8,     8,     8,     8},		/* FPL state */
	/* State 8 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* FPL accepting state with retraction */
	/* State 9 */  { 9,   9,   9,    9,    9,     9,    10,    ER},		/* String Literal state */
	/* State 10 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* String Literal accepting state */
	/* State 11 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		/* Error accepting state */
	/* State 12 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS}		/* Error accepting state with retraction */
};

/* Accepting state table definition */
#define ASWR     13  /* accepting state with retract */
#define ASNR     14  /* accepting state with no retract */
#define NOAS     15  /* not accepting state */

int as_table[] = {
	/* State 0 */ NOAS, /* Not Accepting State */
	/* State 1 */ NOAS, /* Not Accepting State */
	/* State 2 */ ASWR, /* Accepting State With Retract */
	/* State 3 */ ASNR, /* Accepting State No Retract */
	/* State 4 */ NOAS, /* Not Accepting State */
	/* State 5 */ ASWR, /* Accepting State With Retract */
	/* State 6 */ NOAS, /* Not Accepting State */
	/* State 7 */ NOAS, /* Not Accepting State */
	/* State 8 */ ASWR, /* Accepting State With Retract */
	/* State 9 */ NOAS, /* Not Accepting State */
	/* State 10 */ ASNR, /* Accepting State No Retract */
	/* State 11 */ ASNR, /* Accepting State No Retract */
	/* State 12 */ ASWR /* Accepting State With Retract */
};

/* Accepting action function declarations */
Token aa_func02(char* lexeme); /* State 2 accepting function */
Token aa_func03(char* lexeme); /* State 3 accepting function */
Token aa_func05(char* lexeme); /* State 5 accepting function */
Token aa_func08(char* lexeme); /* State 8 accepting function */
Token aa_func10(char* lexeme); /* State 10 accepting function */
Token aa_func11(char* lexeme); /* State 11 accepting function */
Token aa_func12(char* lexeme); /* State 12 accepting function */

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/
typedef Token(*PTR_AAF)(char* lexeme);

/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */
PTR_AAF aa_table[] = {
	/* state 0 */ NULL, /* No function callback */
	/* state 1 */ NULL, /* No function callback */
	/* state 2 */ aa_func02, /* State 2 accepting function callback */
	/* state 3 */ aa_func03, /* State 3 accepting function callback */
	/* state 4 */ NULL, /* No function callback */
	/* state 5 */ aa_func05, /* State 5 accepting function callback */
	/* state 6 */ NULL, /* No function callback */
	/* state 7 */ NULL, /* No function callback */
	/* state 8 */ aa_func08, /* State 8 accepting function callback */
	/* state 9 */ NULL, /* No function callback */
	/* state 10 */ aa_func10, /* State 10 accepting function callback */
	/* state 11 */ aa_func11, /* State 11 accepting function callback */
	/* state 12 */ aa_func12 /* State 12 accepting function callback */
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */
#define KWT_SIZE  10

char* kw_table[] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif
