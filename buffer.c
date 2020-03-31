/*******************************************************************************
Filename: buffer.c
Compiler: Microsoft Visual Studio Enterprise 2019 Version 16.2.4
Author: Andressa Pessoa de Araujo Machado [040923007]
Course: CST8152 - Compilers
Lab Section Number: 012
Assignment Number: 1 - The Buffer
Submission Date: 2020/01/30
Professor's Name: Svillen Ranev

Purpose: Implementation of a buffer that can operate in three different modes:
[F]ixed-size buffer
[A]dditive self-incrementing buffer
[M]ultiplicative self-incrementing buffer
Based on two associated data structures: a Buffer Descriptor and an Array of Characters,
The actual Buffer.

Function List:
	b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(),  b_capacity(),
	b_markc(), b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(),
	b_compact(), b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind().
*******************************************************************************/

#include "buffer.h"

/*******************************************************************************
Function name: b_allocate
Purpose: allocates memory on the heap for the new buffer

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions: calloc(), malloc()
Parameters: short init_capacity, char inc_factor, char o_mode
Return Value: pBD or NULL in case of any error occurs.

Algorithm:	1. Creates a new buffer on the heap using calloc()
			2. Check for parameter`s validation
			3. Initializes the attributes of the buffer
*******************************************************************************/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
	Buffer* pBD;

	//Checking capacity before allocate the character buffer
	//initial capacity must be from 0 to the maximum positive value supported by a short (32,767 - 1).
	if (init_capacity < 0 || init_capacity > MAX_CAPACITY) {
		return NULL;
	}

	if (o_mode != 'a' && o_mode != 'A' && o_mode != 'm' && o_mode != 'M' && o_mode != 'f' && o_mode != 'F') {
		return NULL;
	}

	//Allocates memory for the Buffer structure using calloc(), initializing the allocated memory block to zero.
	//Create a pointer to a new Buffer Descriptor
	pBD = (Buffer*)calloc(1, sizeof(Buffer));

	//Checking if the buffer was not properly created. If not, return NULL.
	if (pBD == NULL) {
		return NULL;
	}

	//modes:
	//[0]: "fixed-size"
	//[1]: "additive self-incrementing"
	//[-1]: "multiplicative self-incremental"

	//Setting respective modes and inc_factor when initial capacity is equal to 0.
	if (init_capacity == 0) {
		init_capacity = DEFAULT_INIT_CAPACITY;
		inc_factor = 15;
	} 
	//Setting respective modes and inc_factor when initial capacity is more than zero:
	if (o_mode == 'f') {
		pBD->mode = FIXED_MODE;
		pBD->inc_factor = 0;
	}
	else if (o_mode == 'a' && (unsigned char)inc_factor <= 255) {
		pBD->mode = ADD_MODE;
		pBD->inc_factor = (unsigned char)inc_factor;
	}
	else if (o_mode == 'm' && (unsigned char)inc_factor <= 100) {
		pBD->mode = MULTI_MODE;
		pBD->inc_factor = (unsigned char)inc_factor;
	}
	else {
		// FAILED modes a, f, and m rules. Free the memory allocated for the Buffer Descriptor.
		free(pBD);
		return NULL;
	
	}

	//Copying the given init_capacity value into the Buffer structure capacity variable.
	pBD->capacity = init_capacity;

	//Setting the flags field to its default value [0xFFFC]
	pBD->flags = DEFAULT_FLAGS;

	/*Dinamically allocating memory to char pointer*/
	pBD->cb_head = (char*)malloc(init_capacity);

	if (pBD->cb_head == NULL) {
		free(pBD);
		return NULL;
	}

	////Returning the pointer to the Buffer structure
	return pBD;
}

/*******************************************************************************
Function name: b_addc
Purpose: To add a new character to the buffer.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions: realloc()
Parameters: pBuffer const pBD, char symbol
Return Value: pBD or NULL in case of any error occurs.

Algorithm:	1. Checks for available space
			2. Add if the buffer is not full
			3. If it is full try to increment capacity based on the mode
			4. Reallocation of memory to hold the new capacity
			5. Update Flag
*******************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol)  {
	//temporary variable to hold the new position in memory for the buffer char array
	char* temp;

	//Checking if the character buffer existis
	if (pBD == NULL) {
		return NULL;
	}

	//Resets the memory allocation flag 
	pBD->flags = pBD->flags & RESET_R_FLAG;

	////If addc_offset is already equals to SHRT_MAX return
	//if (pBD->addc_offset == MAX_CAPACITY) {
	//	return pBD;
	//}

	// Adding when buffer has space, return a pointer to the buffer after
	if ((short)(pBD->addc_offset * sizeof(char)) < pBD->capacity) {
		//treating the pointer as an array to set the new symbol to the next memory available 
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}

	// If the buffer is full, try to increase according to the mode 
	if (b_isfull(pBD)) {
		short new_capacity = 0;

		// mode = 0 is a fixed size, cannot be increased.
		if (pBD->mode == FIXED_MODE) {
			return NULL;
		}

		// mode = 1 is an additive self-incrementing.
		if (pBD->mode == ADD_MODE) {
			//Reached the full capacity, cannot be incremented anymore
			if (pBD->capacity == MAX_CAPACITY) {
				return NULL;
			}

			if (pBD->inc_factor == 0) {
				return NULL;
			}

			//Calculate the new capacity 
			new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor * sizeof(char);

			//new_capacity exceed the maximum allowed value, assign it to the maximum allowed positive value
			if (new_capacity > MAX_CAPACITY) {
				new_capacity = MAX_CAPACITY;
			}

			//The result of the calculation was negative, return null 
			if (new_capacity < 0) {
				return NULL;
			}
		}

		// Multiplicative Mode
		if (pBD->mode == MULTI_MODE) {
			//Buffer reached the maximum capacity, return null
			if (pBD->capacity == MAX_CAPACITY) {
				return NULL;
			}

			//Temporary variables created to calculate the new capacity in the multiplicative mode.
			short available_space = (MAX_CAPACITY)-pBD->capacity;
			short new_inc_factor = (short) (available_space * (((float)pBD->inc_factor) / 100.0f));
			//holds the possible new capacity. 
			new_capacity = (short)(pBD->capacity + new_inc_factor);

			if (new_inc_factor == 0) {
				new_capacity = MAX_CAPACITY;
			}
		}

		//reallocate the content to a new  block of memory and assign to temp. Avoid losing content if realloc fails
		temp = (char*)realloc(pBD->cb_head, new_capacity);

		//If the reallocation was not successful, return. set that pointer to the head of the buffer (cb_head)
		if (temp == NULL) {
			return NULL;
		}

		pBD->capacity = new_capacity;
		//If the reallocation was successful, set that pointer to the head of the buffer (cb_head)
		pBD->cb_head = temp;
		//if reached this point of the code, means it was successful, sets flag for reallocation of memory
		pBD->flags = pBD->flags | SET_R_FLAG;
		//add the new symbol and increase the next address to be used in future adition
		pBD->cb_head[pBD->addc_offset++] = symbol;
	}

	//return pointer to the buffer
	return pBD;
}

/*******************************************************************************
Function name: b_clear
Purpose: retains the memory space currently allocated to the buffer, but re-initializes
		 all appropriate data members of the given Buffer structure (buffer descriptor) so that
		 the buffer will appear as just created to the client functions

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [0] for success or [-1] for failure

Algorithm:	1. Resets addc_offset - Distance from the beginning of the character array
			   to the next add location
			2. Resets getc_offset - Distance from the beginning of the character array
			   to the very last char stored
			3. Resets markc_offset -  Distance from the beginning of the character array
			   to the location of a mark. Mark is a location which indicates the position of
			   a specific character.
			4. Resets the flag to the default value - 0xFFF9
*******************************************************************************/
int b_clear(Buffer* const pBD) {
	//Checks if the buffer exists 
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = pBD->flags & RESET_R_FLAG;
	return 0;
}

/*******************************************************************************
Function name: b_free
Purpose: De-allocates the memory occupied by the character buffer and the Buffer
		 strucutre.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions: free()
Parameters: Buffer* const pBD
Return Value: void

Algorithm:	1. Checks for validity
			2. Calls function free
*******************************************************************************/
void b_free(Buffer* const pBD) {
	//checks if the buffer exists in memory 
	if (!pBD) {
		return;
	}

	free(pBD->cb_head);
	free(pBD);
	return;
}

/*******************************************************************************
Function name: b_isfull
Purpose: Checks if the buffer is full or not.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [1] if the buffer is full, [0] if it is not full and [-1] if a run-time
			 error occurs

Algorithm:	1. Checks for validity
			2. Checks whether the buffer is full or not
*******************************************************************************/
int b_isfull(Buffer* const pBD) {
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//If the next add location is the same value as capacity, means the buffer is full
	if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity) {
		return 1;
	}
	else {
		return 0;
	}
}

/*******************************************************************************
Function name: b_limit
Purpose: Checks the amount of space measured in chars that is current being used by all added characters.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: Distance between the start of the buffer to the next available add position

Algorithm:	1. Checks for validity
			2. Returns the current limit of the character buffer
*******************************************************************************/
short b_limit(Buffer* const pBD) {
	//Check if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//The current limit is the amount of space measured in chars that is currently being used by all added(stored) characters.
	return pBD->addc_offset;
}

/*******************************************************************************
Function name: b_capacity
Purpose: Checks the capacity of the character buffer

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: The capacity of the character buffer

Algorithm:	1. Checks for validity
			2. Returns the current capacity
*******************************************************************************/
short b_capacity(Buffer* const pBD) {
	//Checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//if (!pBD->capacity || pBD->capacity < 0) {
	//	return RT_FAIL_1;
	//}

	return pBD->capacity;
}

/*******************************************************************************
Function name: b_markc
Purpose: To return a pointer to the location of a specific character in the buffer.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: pBuffer const pBD, short mark
Return Value: A pointer to the location of the currently set markc_offset or NULL
			  if a run-time error occurs

Algorithm:	1. Checks for validity
			2. Returns a pointer to the location of a specific character in the buffer
*******************************************************************************/
char* b_markc(pBuffer const pBD, short mark) {
	//checks if the buffer exists in memory
	if (pBD == NULL) {
		return NULL;
	}

	//a mark must be in the current limit of the buffer
	if (mark < 0 || mark > pBD->addc_offset) {
		return NULL;
	}

	pBD->markc_offset = mark;
	return &pBD->cb_head[pBD->markc_offset];
}

/*******************************************************************************
Function name: b_mode
Purpose: Give the mode in which the buffer is opperating

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: pBuffer const pBD
Return Value: An int value representing the operational mode.
			  [0] fixed-size mode
			  [1] additive self-incrementing mode
			  [-1] multiplicative self-incrementing mode

Algorithm:	1. Checks for validity
			2. Returns an int value representing the operational mode
*******************************************************************************/
int b_mode(Buffer* const pBD) {
	//checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	return (int)pBD->mode;
}

/*******************************************************************************
Function name: b_incfactor
Purpose: Gives the buffer`s increment factor according to its operational mode

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: pBuffer const pBD
Return Value: An int value representing the increment factor of the buffer.

Algorithm:	1. Checks for validity
			2. Returns an int value representing the increment factor of the buffer.
*******************************************************************************/
size_t b_incfactor(Buffer* const pBD) {
	//checks if the buffer exits in memory or if the increment factor is invalid
	if (pBD == NULL) {
		return INC_FACTOR_ERROR;
	}
	return (unsigned char)pBD->inc_factor;
}

/*******************************************************************************
Function name: b_load
Purpose: Reads one char at a time from an open input file specified by the fi constant
		 into the buffer

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:fgetc(), feof(), b_addc()
Parameters: FILE* const fi, Buffer* const pBD
Return Value: an int value representing the number of chars added, [-1] for run time error,
			 [-2] for full buffer

Algorithm:	1. Checks for validity
			2. Adds one character to the buffer at a time
			3. Returns the number of characters added to the buffer
*******************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD) {
	//Checks if the buffer exists in memory and if there is ant file to be read
	if (pBD == NULL || fi == NULL) {
		return RT_FAIL_1;
	}

	//temp variable to hold each char being read from file to buffer
	char c;
	//counter for number of characters added to the buffer
	int count = 0;

	//indefinite ending loop because we dont have the exact break condition  
	while (1) {
		//Getting current char 
		c = (char)fgetc(fi);

		//test end of file, if true, leaves the loop, returns count 
		if (feof(fi)) {
			return count;
		}

		//Tries to add char to the buffer, if successful, increase counter
		if (b_addc(pBD, c) == NULL) {
			//if could not add, returns a character to the file stream, returns error.
			ungetc(c, fi);
			return LOAD_FAIL;
		}
		//increase the character counter
		count++;
	}
}

/*******************************************************************************
Function name: b_isempty
Purpose: To check if the buffer is empty, in other words, if there is no characters added
		 to the buffer.

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [1] for empty [0] if it is not empty [-1] if a run time error occurs.

Algorithm:	1. Checks for validity
			2. Checks the value of the addc_offset
			3. Return 0 or 1 depending if the buffer is empty or not
*******************************************************************************/
int b_isempty(Buffer* const pBD) {
	//check if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//checks the value of the addc_offset. That variable holds the index of the buffer that will be used 
	//to store the character 
	if (pBD->addc_offset == 0) {
		return 1;
	}
	else {
		return 0;
	}
}

/*******************************************************************************
Function name: b_getc
Purpose: To read the buffer

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [1] for empty [0] if it is not empty [-2] if a run time error occurs.

Algorithm:	1. Checks for validity
			2. Checks if addc_offset and getc_offset are equal
			If they are equals, set the EOB bit to 1, otherwise set the EOB bit to 0.
			3. Increments getc_offset by 1
			4. Return character located at getc_offset
*******************************************************************************/
char b_getc(Buffer* const pBD) {
	//Checks if the buffer exists in memory 
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	//Check for equalty. Means that if positive, we have reached the end of the buffer
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags = pBD->flags | SET_EOB; //set to 1 
		return 0;
	}
	else {
		pBD->flags = pBD->flags & RESET_EOB; //set to 0
	}

	//If not at the end of the buffer, return the char at that location and increase the getc_offset by one
	return pBD->cb_head[pBD->getc_offset++];
}

/*******************************************************************************
Function name: b_eob
Purpose: Gives the value of the EOB bit

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: EOB bit value or  [-1] if a run time error occurs.

Algorithm:	1. Checks for validity
			2. Using a bitwise operation, return the value of the EOB bit
*******************************************************************************/
int b_eob(Buffer* const pBD) {
	//Checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//bitwise operation to return the value of the EOB bit
	return pBD->flags & CHECK_EOB;
}

/*******************************************************************************
Function name: b_print
Purpose: Prints character by character the contents of the character buffer to the
		 standar output

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions: b_getc(), printf()
Parameters: Buffer* const pBD, char nl
Return Value: number of the characters printed and [-1] on failure.

Algorithm:	1. Checks for validity
			2. Prints the content of the buffer calling b_getc() while not the end of the buffer
			3. checks for the new line character, if not null, print a new line.
*******************************************************************************/
int b_print(Buffer* const pBD, char nl) {
	//checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//Checks if the array of characters exists
	if (pBD->cb_head == NULL) {
		return RT_FAIL_1;
	}

	// char used to hold the current char returned by the b_getc()
	char to_be_read;

	// Holds the number of characters printed
	int char_counter = 0;

	//indefinite ending loop because we dont have the exact break condition  
	while (1) {
		//Getting current char 
		to_be_read = b_getc(pBD);

		//while the char is not null, print it.
		if (b_eob(pBD)) {
			break;
		}
		else {	
			char_counter++;
			printf("%c", to_be_read);
		}
	}

	if (nl != 0){
		printf("\n");
	}

	return char_counter;
}

/*******************************************************************************
Function name: b_compact
Purpose: Sets specified character to be the last one in the buffer, adjusting the capacity

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions: realloc()
Parameters: Buffer* const pBD, char symbol
Return Value: pointer to the buffer or NULL for failure

Algorithm:	1. Checks for validity
			2. Calculates new capacity
			3. Reallocates the content of the buffer to the new capacity
			4. Puts the symbol in the last position of the buffer
			5. Updates the buffer structure fields
*******************************************************************************/
Buffer* b_compact(Buffer* const pBD, char symbol) {
	//Checks if the buffer exists in memory
	if (pBD == NULL || pBD->cb_head == NULL) {
		return NULL;
	}

	//pBD->flags = pBD->flags & RESET_R_FLAG;

	//holds the new capacity
	short new_capacity = (short)(sizeof(char) * (pBD->addc_offset + 1));

	//Making sure we are not underflowing the new_capacity value with the operation
	if (new_capacity <= 0) {
		return NULL;
	}

	//Temporary variable to avoid losing buffer content 
	char* temp;
	temp = (char*)realloc(pBD->cb_head, new_capacity);

	//Checking if the realloc was sucessful  
	if (temp == NULL) {
		return NULL;
	}

	//Sets the buffer to the new allocated size
	pBD->cb_head = temp;

	//Perfoms updates to the buffer structure fields
	pBD->capacity = new_capacity;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	//if reached this point of the code, means it was successful, sets flag for reallocation of memory
	pBD->flags = pBD->flags | RESET_R_FLAG;

	//returns the buffer pointer
	return pBD;
}

/*******************************************************************************
Function name: b_rflag
Purpose: Gets the value of the r_flag bit

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [-1] for failure, char representing the value of the specific bit

Algorithm:	1. Checks for validity
			2. Performs bitwise operation to return the correct bit that is set
*******************************************************************************/
char b_rflag(Buffer* const pBD) {
	//Checks if the buffer exists in memory 
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return pBD->flags & CHECK_R_FLAG;
}

/*******************************************************************************
Function name: b_retract
Purpose: Decrements the read counter by one

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [-1] for failure, short representing the new getc_offset

Algorithm:	1. Checks for validity
			2. Decrements the value of the getc_offset variable and return it.
*******************************************************************************/
short b_retract(Buffer* const pBD) {
	//Checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	if (pBD->getc_offset == 0) {
		return RT_FAIL_1;
	}

	//decrements and sets back
	--pBD->getc_offset;
	return pBD->getc_offset;
}

/*******************************************************************************
Function name: b_reset
Purpose: Sets the read position to a previously marked position in the buffer

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [-1] for failure, short representing the new getc_offset

Algorithm:	1. Checks for validity
			2. Updates the value of the getc_offset variable to the markc_offset
			and return it.
*******************************************************************************/
short b_reset(Buffer* const pBD) {
	//Checks if the buffer exists in memory 
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/*******************************************************************************
Function name: b_getcoffset
Purpose: Returns the read position

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [-1] for failure, short representing the current getc_offset

Algorithm:	1. Checks for validity
			2. Returns the value of getc_offset
*******************************************************************************/
short b_getcoffset(Buffer* const pBD) {
	//Checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//return the value of getc_offset stored in the buffer
	return pBD->getc_offset;
}

/*******************************************************************************
Function name: b_rewind
Purpose: Resets the read position and the custom mark position to zero

Author: Andressa Machado [040923007]
History/Versions: 2.1

Called functions:
Parameters: Buffer* const pBD
Return Value: [-1] for failure, [0] for success

Algorithm:	1. Checks for validity
			2. Sets the value to zero and resets the EOB BIT
*******************************************************************************/
int b_rewind(Buffer* const pBD) {
	//Checks if the buffer exists in memory
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	//Perfoms resets 
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;

	return 0;
}
