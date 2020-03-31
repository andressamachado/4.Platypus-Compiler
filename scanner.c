/****************************************************************************************************
Filename:				table.h
Compiler:				Microsoft Visual Studio Enterprise 2019 Version 16.2.4
Author:					Andressa Pessoa de Araujo Machado [040923007] & Raphael Guerra [040908555]
Course:					CST8152 - Compilers
Lab Section Number:		012 &  011
Assignment Number:		2 - The Scanner
Submission Date:		23.March.2020
Professor's Name:		Svillen Ranev

Purpose:				A lexical analyzer for the PLATYPUS Language. The scanner reads a source code 
						from a text file and produces a stream of token representations.

Function List:			scanner_init(), malar_next_token(), get_next_state(), char_class(), aa_func02(), aa_func03(),
						aa_func08(), aa_func05(), aa_func10(), aa_func11(), aa_func12(), iskeyword()
****************************************************************************************************/


/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#define DEBUG  /* for conditional processing */
#undef  DEBUG

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h" /* buffer functions and constants */
#include "token.h" /* token struct and constants */
#include "table.h" /* finite state constants and tables */

/* Global objects - variables */
/* This buffer is used as a repository for string literals. It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */

/*******************************************************************************
Function name:		scanner_init
Purpose:			Initializes scanner
Author:				Svillen Ranev
History/Versions:	1.0
Called functions:	b_isempty(), b_rewind(), b_clear()
Parameters:			short init_capacity, char inc_factor, char o_mode
Return Value:		int 1 for failure and int 0 for success

Algorithm:			1. Checks if the buffer is created
					2.	
					3.

*******************************************************************************/
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*******************************************************************************
Function name:		malar_next_token
Purpose:			Match-a-Lexeme-and-Return. The purpose of this function is token recognition.
Author:				Svillen Ranev, Andressa Machado & Raphael Guerra
History/Versions:	1.0
Called functions:	b_getc(), isspace(), b_retract(), b_markc(), b_getcoffset(), b_reset(), isalnum(), 
					b_getcoffset(), get_next_state(), b_allocate(), b_addc(), b_compact(), b_free()
Parameters:			void
Return Value:		A recognized platypus Token containing its code and its attribute, 
					or an invalid Token containing an error code.   

Algorithm:			1. Infinite loop takes character by character to recognize a token.
					2. if it gets a space, increase the line counter and go back to the beginning of the loop.
						skipping spaces.
					3. Recognizes the symbol and return the token with its code and attribute
					4. if Source end-of-file(SEOF), return the related code and attribute
					5. For the case when the token is composed of two symbols, check the next one and then
						call the function b_retract() before returning the token. It will allow us to continue read 
						the source code until the end of the file.
					6. For the case when the token is bigger than that (e.g. .AND.), uses the b_markc() function to
						mark the point to retract, and b_reset() to retract. 
					7. Implementation of Finite State Machine (DFA) or Transition Table driven Scanner for the 
						recognition of special tokens (Based on states)
*******************************************************************************/
Token malar_next_token(void) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	
	char tempChar; /*temporary holding place for the 'c' input symbol*/
	int acceptingState = NOAS; /* type of state - initially not accepting */

	while (1) { /* endless loop broken by token returns it will generate a warning */
		
		c = b_getc(sc_buf);/* get the next symbol from the input buffer */
		
		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */

		if (isspace(c)) { /* check if character is a white-space */
			if (c == '\n') /* if character is a line terminator */
				line++; /* add 1 to current line */
			continue; /* go to next line */
		}

		/* SET TOKEN CODE AND TOKEN ATTRIBUTE(IF AVAILABLE) */
		switch (c) {

		case SEOF: /* source end of file case */
			t.code = SEOF_T; /* set end of file token code */
			t.attribute.seof = SEOF; /* set -1 as attribute */
			return t; /* return token */

		case SEOF_2: /* source end of file case */
			t.code = SEOF_T; /* set end of file token code */
			t.attribute.seof = SEOF_2; /* set -1 as attribute */
			return t; /* return token */

		case '\0': /* null terminator case */
			t.code = SEOF_T; /* set end of file token code */
			t.attribute.seof = SEOF_0; /* set 0 as attribute */
			return t; /* return token */

		case '!': /* comment symbol */
			tempChar = c = b_getc(sc_buf);/* get next character from input buffer */
			
										  /* if next char is end of file */
			if (tempChar == '\0' || tempChar == SEOF || tempChar == SEOF_2 ) {
				t.code = ERR_T;/* set 0 token code, Error */
				t.attribute.err_lex[0] = '!';/* set the character as lex error attribute */
				t.attribute.err_lex[1] = '\0';/* set end of string to the lex error attribute */
				return t;
			}
			else if (tempChar != '!') { /* if next char is different */
				t.code = ERR_T;/* set 0 token code, Error */
				t.attribute.err_lex[0] = '!';/* set the character as lex error attribute */
				
				if (c != '\n') { /* if next char is not a line terminator */
					t.attribute.err_lex[1] = c;/* set the character to lex error attribute array*/
				}
				t.attribute.err_lex[2] = '\0';/* set end of string to the lex error attribute */
			}

			/* loop until character is a line terminator */
			while (c != '\n') {
				c = b_getc(sc_buf);/* get next character from input buffer */
				
				/* if next char is end of file */
				if (c == '\0' || c == SEOF || c == SEOF_2) {
					t.code = SEOF_T;/* set end of file token code */
					return t;/* return token */
				}
			}

			line++;/* add 1 for next line */
			
			/* if next char is not a comment */
			if (tempChar != '!') {
				return t; /* return token */
			}

			/* continue to check next line */
			continue;/* go to the next line */

		case '{': /* left bracket symbol */
			t.code = LBR_T; /* set attribute as left bracket token */
			return t; /* return token */

		case '}': /* right bracket symbol */
			t.code = RBR_T; /* set attribute as right bracket token */
			return t; /* return token */

		case '(': /* left parenthesis symbol */
			t.code = LPR_T; /* set attribute as left parenthesis */
			return t; /* return token */

		case ')': /* right parenthesis symbol */
			t.code = RPR_T; /* set attribute as right parenthesis */
			return t; /* return token */

		case '<': /* Less than symbol */
			c = b_getc(sc_buf); /* get next character from buffer */
			
			if (c == '>') { /* if next char is Greater than */
				t.attribute.rel_op = NE; /* set 1 as attribute, NotEqual operation */
				t.code = REL_OP_T; /* set relational operator token code */
				return t; /* return token */
			}

			b_retract(sc_buf); /* retract the buffer */
			
			t.code = REL_OP_T; /* set relational operator token code */
			t.attribute.rel_op = LT; /* set 3 as attribute, LowerThan */
			return t; /* return token */

		case '>': /* Greater than symbol */
			t.code = REL_OP_T; /* set relational operator token code */
			t.attribute.rel_op = GT; /* set 3 as attribute, GreaterThan */
			return t;/* return token */

		case '#': /* hash symbol */
			tempChar = b_getc(sc_buf); /* get next character from buffer */
			
			if (tempChar != '#') { /* if next char is different */
				t.code = ERR_T; /* set Error token code*/
				t.attribute.err_lex[0] = c; /* character set as lex error attibute */
				t.attribute.err_lex[1] = '\0'; /* set end of string to the lex arror attribute */
				b_retract(sc_buf); /* retract the buffer */
				return t;/* return token */
			}
			
			t.code = SCC_OP_T;/* set string concatenation operator token code */
			return t;/* return token */

		case ';': /* semi-colon symbol */
			t.code = EOS_T; /* set EndOfStatement token code, semi-colon ";" */
			return t; /* return token */

		case ',': /* comma symbol */
			t.code = COM_T; /* set comma token code, "," */
			return t; /* return token */

		case '-': /* minus symbol */
			t.code = ART_OP_T; /* set Arithmetic operator token code */
			t.attribute.arr_op = MINUS; /* set 1 as attribute, MINUS */
			return t; /* return token */

		case '+': /* plus symbol */
			t.code = ART_OP_T; /* set Arithmetic operator token code */
			t.attribute.arr_op = PLUS; /* set 0 as attribute, PLUS */
			return t; /* return token */

		case '/': /* division symbol */
			t.code = ART_OP_T; /* set Arithmetic operator token code */
			t.attribute.arr_op = DIV; /* set 3 as attribute, DIVISION */
			return t; /* return token */

		case '*': /* multiplication symbol */
			t.code = ART_OP_T; /* set Arithmetic operator token code */
			t.attribute.arr_op = MULT; /* set 2 as attribute, MULTIPLICATION */
			return t; /* return token */

		case '=':/* equal symbol */
			if (b_getc(sc_buf) == '=') { /* if next char is the same */
				t.code = REL_OP_T;/* set relational operator token code */
				t.attribute.rel_op = EQ; /* set 3 as attribute, Equals */
				return t;/* return token */
			}

			b_retract(sc_buf);/* retract the buffer */
			t.code = ASS_OP_T;/* set Assignment operator token code */
			return t;/* return token */

		case '.': /* dot symbol */
			b_markc(sc_buf, b_getcoffset(sc_buf)); /* mark location for possible retraction */
			
			c = b_getc(sc_buf); /* get next character from input buffer */
			
			/* if next character is a logical operator (.AND.) */
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T; /* set 11 as logical operator token code, .AND. */
				t.attribute.log_op = AND; /* set 0 as logical operator attribute, .AND. */
				return t; /* return token */
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') { /* if next character is a logical operator (.OR.) */
				t.code = LOG_OP_T; /* set 11 as logical operator token code, .AND. */
				t.attribute.log_op = OR; /* set 1 as logical operator attribute, .AND. */
				return t; /* return token */
			}
			else { /* anything else */
				b_reset(sc_buf); /* function call to reset input buffer */
				t.code = ERR_T; /* set 0 token code, Error */
				t.attribute.err_lex[0] = '.'; /* set the character to lex error attribute array*/
				t.attribute.err_lex[1] = '\0'; /* set end of string to the lex error attribute */
				return t; /* return token */
			}

		/* Part 2: Implementation of Finite State Machine (DFA) or Transition Table driven Scanner
		Note: Part 2 must follow Part 1 to catch the illegal symbols
		*/

		default:
			/* if character is alfa-numeric or string literal or line terminator */
			if (isalnum((int)c) != 0 || c == '"' || c == '\0') {
				/* set start offset of a lexeme as the input buffer last index minus 1 for current char*/
				lexstart = b_getcoffset(sc_buf) - 1;
				/* call get_next_state function to set new state */
				state = get_next_state(state, c);
				
				/* loop until accepting state change to ASWR or ASNR */
				while (acceptingState == NOAS) {
					c = b_getc(sc_buf); /* get next character from input buffer */
					state = get_next_state(state, c); /* call get_next_state function to set new state */
					acceptingState = as_table[state]; /* set acceptingState from state table position */
				}

				/* if new accepting state needs to retract the input buffer */
				if (acceptingState == ASWR) {
					b_retract(sc_buf); /* retract to last character */
				}

				/* set end offset of a lexeme as the input buffer offset*/
				lexend = b_getcoffset(sc_buf);
				/* create a temporary lexeme buffer */
				lex_buf = b_allocate(lexend - lexstart, 0, 'f');
				
				/* if memory was not allocated */
				if (!lex_buf) {
					t.code = ERR_T; /* set 0 token code, Error */
					scerrnum = 1; /* set scerrnum to 1, true value */
					
								  /* lexeme error statement */
					for (int i = 0; i < (int)strlen("RUN TIME ERROR: "); i++) {
						t.attribute.err_lex[i] = "RUN TIME ERROR: "[i];
					}
					return t; /* return token */
				}

				/* mak input buffer offset to lexstart */
				b_markc(sc_buf, lexstart);
				/* reset input buffer */
				b_reset(sc_buf);
				/* loop to populate lexeme buffer */
				
				for (int i = lexstart; i < lexend; i++)
					b_addc(lex_buf, b_getc(sc_buf)); /* add char to lexeme buffer from input buffer */
				
				b_compact(lex_buf, '\0');/* compact lexeme buffer */
				t = aa_table[state](b_markc(lex_buf, 0));/* call apropriate accepting functions with state index */
				b_free(lex_buf);/* free lexeme buffer allocated memory */
				
				return t; /* return token */
			}
			break; /* end of default case */
		}

		/* illegal symbol inputed */
		t.code = ERR_T;/* set 0 token code, Error */
		t.attribute.err_lex[0] = c;/* set the character to lex error attribute array*/
		t.attribute.err_lex[1] = '\0';/* set end of string to the lex error attribute */
		
		return t;/* return token */
	}/* end while(1) */
}

/*******************************************************************************
Function name:		get_next_state
Purpose:			
Author:				Svillen Ranev
History/Versions:	1.0
Called functions:	char_class(), assert(), exit(), printf()
Parameters:			int state, char c
Return Value:		An int representing the next state to go according to the as_table[] in the table.h

Algorithm:			1. Calls char_class() to get the symbol (represented by the column in the matriz) 
					2. set variable next to the value found in the matriz (representing the next state)
*******************************************************************************/
int get_next_state(int state, char c) {
	int col; /* column index */
	int next; /* next state */
	col = char_class(c); /* sets column index with the returning position of char_class function */
	next = st_table[state][col]; /* set next state from current state table index of the column */

#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:
	Assertion failed: test, file filename, line linenum
	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*******************************************************************************
Function name:		char_class
Purpose:			Find the column number in the transition table
Author:				Raphael Guerra
History/Versions:	1.0
Called functions:	isdigit() 
Parameters:			char c to be evaluated and found in the table
Return Value:		An int representing the column number in the transition table st_table for the input character

Algorithm:			1. Gets the char and process to get the column
					2. return the variable representing the column
*******************************************************************************/
int char_class(char c) {
	int val; /* variable to set an integer according to parameter character class */

	/* [a-zA-Z] */
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
		val = 0; /* column 0 */
	}
	/* [0] */
	else if (c == '0') {
		val = 1; /* column 1 */
	}
	/* [1-9] */
	else if (isdigit(c) && c != '0') {
		val = 2; /* column 2 */
	}
	/* [.] */
	else if (c == '.') {
		val = 3; /* column 3 */
	}
	/* [#] */
	else if (c == '#') {
		val = 4;/* column 4 */
	}
	/* ["] */
	else if (c == '"') {
		val = 6; /* column 6 */
	}
	/* End Of File */
	else if (c == SEOF_2 || c == SEOF || c == '\0') {
		val = 7; /* column 7 */
	}
	/* any other symbol */
	else {
		val = 5;
	}
	return val; /* return respective column integer*/
}

/*******************************************************************************
Function name:		aa_func02
Purpose:			Accepting function for the Arithmetic Variable Identifier and Keywords (VID - AVID/KW)
Author:				Andressa Machado
History/Versions:	1.0
Called functions:	iskeyword(), strlen(), strcpy()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated 

Algorithm:			1. Recognizes as a keyword or not
					2. If variable bigger than 8, stores only the first 8.
					3. If smaller than 8 stores the whole name
*******************************************************************************/
Token aa_func02(char lexeme[]) {
	Token t = { 0 }; /* Token to be returned*/
	int i = 0; /* temporary integer for iteration */

	if ((t.attribute.kwt_idx = iskeyword(lexeme)) != RT_FAIL_1) {	/* if lexeme is a keyword */
		t.code = KW_T; /* set 16 for token code, keyword token */
		return t; /* return token */
	}
	else {	/* if lexeme is not a keyword */
		t.code = AVID_T; /* set 2 for token code, variable identifier token */
	}

	if (strlen(lexeme) > VID_LEN) {	/* if lexeme is longer than 8 */
		for (i = 0; i < VID_LEN; i++) {
			t.attribute.vid_lex[i] = lexeme[i]; /* store only the first 8 characters */
		}
		t.attribute.vid_lex[VID_LEN] = '\0'; /* append end of string */
	}
	else { /* if lexeme is smaller than 8 */
		strcpy(t.attribute.vid_lex, lexeme);/* store the whole array */
	}

	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func03
Purpose:			Accepting function for the string variable identifier (VID - SVID)
Author:				Raphael Guerra
History/Versions:	1.0
Called functions:	strlen(), strncpy()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. Checks the size of the lexeme
					2. If variable bigger than 8, stores only the first 8.
					3. If smaller than 8 stores the whole name
*******************************************************************************/
Token aa_func03(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	int i = 0; /* temporary integer for iteration */

	t.code = SVID_T; /* set 3 for token code, String variable identifier token */

	if (strlen(lexeme) > VID_LEN) { /* if lexeme is longer than 8 */
		for (i = 0; i < VID_LEN - 1; i++) {
			t.attribute.vid_lex[i] = lexeme[i];/* store only the first 8 characters */
		}

		t.attribute.vid_lex[VID_LEN - 1] = '#'; /* append # */
		t.attribute.vid_lex[VID_LEN] = '\0'; /* append end of string */
	}
	else { /* if lexeme is smaller than 8 */
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));/* store the whole array */
		t.attribute.vid_lex[strlen(lexeme)] = '\0';/* append end of string */
	}

	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func05
Purpose:			Accepting function for the floating-point literal (FPL)
Author:				Andressa Machado
History/Versions:	1.0
Called functions:	atof()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. Converts the lexeme to a floating point value
					2. Checks the range of the float value
					3. If bigger, overflow.
					4. Check the Error lexeme and if it is longer than 20. First 17 are stored and then "..." is concatenated
					5. Sets the appropriated code and attribute
					6. Returns the token
*******************************************************************************/
Token aa_func05(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	long val = atol(lexeme); /* hold converted lexeme into long */
	
	if (!t.attribute.vid_lex) { /* if there is no identifier */
		scerrnum = 5; /* set run time error value */
		aa_func12("RUN TIME ERROR!"); /* call error accepting function */
	}

	if (val >= 0 && val <= SHRT_MAX) { /* if value is in 2-byte float range*/
		t.code = INL_T; /* set 5 for token code, Integer Literal token */
		t.attribute.int_value = val; /* set lexeme into integer value attribute */
	}
	else { /* if value is not in 2-byte float range */
		t.code = ERR_T;/* set 0 token code, Error */
		
		if (strlen(lexeme) > ERR_LEN) {/* if lexeme is longer than 20 */
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3); /* store the first 17 characters */
			/* append 3 dots lexeme error */
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
		}
		else { /* if lexeme is smaller than 20 */
			strcpy(t.attribute.err_lex, lexeme);/* store the whole array */
		}
	}

	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func08
Purpose:			Accepting function for the floating-point literal (FPL)
Author:				Raphael Guerra
History/Versions:	1.0
Called functions:	atof()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. Converts the lexeme to a floating point value
					2. Checks the range of the float value
					3. If bigger, overflow. 
					4. Check the Error lexeme and if it is longer than 20. First 17 are stored and then "..." is concatenated
					5. Sets the appropriated code and attribute
					6. Returns the token
*******************************************************************************/
Token aa_func08(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	int i;/* temporary integer for iteration */
	double val = atof(lexeme); /* hold converted lexeme into double */

	if ((val > 0 && val < FLT_MIN) || val > FLT_MAX) { /* if value is not into 4-byte float range*/
		t = aa_table[ES](lexeme); /* store overflow error */
		
		return t; /* return token */
	}
	else if (strlen(lexeme) > ERR_LEN) {	/* if lexeme is longer than 20 */
		for (i = 0; i < ERR_LEN - 3; i++) { 	/* store the first 17 characters */
			t.attribute.err_lex[i] = lexeme[i];
			/* append 3 dots lexeme error */
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
		}
	}

	t.code = FPL_T; /* set 4 for token code, Floating point Literal token */
	t.attribute.flt_value = (float)val; /* set float converted lexeme into float value attribute */
	
	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func10
Purpose:			Accepting function for the string literal(SL)
Author:				Andressa Machado
History/Versions:	1.0
Called functions:	b_limit(), strlen(), b_addc()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. Sets the token attribute to the offset from the beggining of the str_LBT char buffer
						to the location where the first char of the lexeme content will be added to the buffer
					2. During the process of copying, the double quotation marks should be ignored
					3. Appends the terminator
					4. sets the code for string literals
					5. Return token
*******************************************************************************/
Token aa_func10(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	int i = 0; /* temporary integer for iteration */

	t.attribute.str_offset = b_limit(str_LTBL); /* set string offset with current limit */
	
	for (i = 0; i < (int)strlen(lexeme); i++) { /* iterating lexeme array for copying */
		if (lexeme[i] == '\n') { /* if line terminator */
			line++; /* increment one for next line */
		}

		if (lexeme[i] != '"') { /* if not quotation mark (to be ignored) */
			b_addc(str_LTBL, lexeme[i]); /* copy char to string lieral repository */
		}
	}

	b_addc(str_LTBL, '\0');/* append end of string */
	t.code = STR_T; /* set 6 for token code, String Literal token */
	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func11
Purpose:			Accepting function for the Error token
Author:				Raphael Guerra
History/Versions:	1.0
Called functions:	strlen()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. Checks for the size of the error lexeme
					2. if smaller than 20, stores the whole lexeme
					3. if bigger than 20, only the first 17 should be stored than "..." should be appended
					4. checks for line terminator, if found, line counter is incremented
					5. Sets the code
					6. Return the token
*******************************************************************************/
Token aa_func11(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	int i = 0;/* temporary integer for iteration */

	if ((int)strlen(lexeme) <= ERR_LEN - 3) { /* if lexeme is smaller than 17 */
		strcpy(t.attribute.err_lex, lexeme); /* store the whole array */
		t.attribute.err_lex[ERR_LEN] = '\0';/* append end of string */
	}
	else {/* if lexeme is smaller than 20 */
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);/* store the first 17 characters */
		/* append 3 dots lexeme error */
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
	}

	for (i = 0; i < (int)strlen(lexeme); i++) { /* iterate to check for line terminators */
		if (lexeme[i] == '\n') /* if lexeme char equals to line terminator */
			line++; /* increment one for next line */
	}

	t.code = ERR_T;/* set 0 token code, Error */
	return t;/* return token */
}

/*******************************************************************************
Function name:		aa_func12
Purpose:			Accepting function for the error state with no retract
Author:				Andressa Maachado
History/Versions:	1.0
Called functions:	none
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. sets token to error state with no retract
*******************************************************************************/
Token aa_func12(char lexeme[]) {
	Token t = { 0 }; /*Token to be returned*/
	t = aa_table[ES](lexeme); /* set token to error state with no retract */
	
	return t;/* return token */
}

/*******************************************************************************
Function name:		isKeyword
Purpose:			Checks for lexeme
Author:				Raphael Guerra
History/Versions:	1.0
Called functions:	b_limit(), strlen(), b_addc()
Parameters:			char lexeme[] to be processed
Return Value:		A token generated

Algorithm:			1. iterate keywords table to check for lexeme
					2. Return keywords index in the table
					3. Or -1 if not found
*******************************************************************************/
int iskeyword(char* kw_lexeme) {
	int i = 0;/* temporary integer for iteration */
	
	for (i; i < KWT_SIZE; i++) { /* iterate keywords table to check for lexeme */
		if (strcmp(kw_table[i], kw_lexeme) == 0)/* compare lexeme to each keyword */
			return i; /* if matches return keyword index */
	}

	return -1;/* return -1 if no keyword matched */
}

