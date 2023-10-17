/* date.x - Specification of remote date and time service
bindate() which returns the binary time and date (no args).
This file is the input to rpcgen */


program DATEPROG { /* remote program name (not used)*/
version DATEVERS { /* declaration of program version
number*/
long BINDATE(void) = 1; /* procedure number = 1 */
} = 1; /* definition of program version = 1*/
} = 0x3012225; /* remote program number (must be
unique)*/
