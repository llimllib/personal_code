/* chatclient.c */

#include <stdlib.h>
#include <stdio.h>
#include <cnaiapi.h>

#define BUFFSIZE		256
#define INPUT_PROMPT		"Input   > "
#define RECEIVED_PROMPT		"Received> "

int recvln(connection, char *, int);
int readln(char *, int);

/*-----------------------------------------------------------------------
 *
 * Program: chatclient
 * Purpose: contact a chatserver and allow users to chat
 * Usage:   chatclient <compname> <appnum>
 *
 *-----------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
	computer	comp;
	connection	conn;
	char		buff[BUFFSIZE];
	int		len;

	if (argc != 3) {
		(void) fprintf(stderr, "usage: %s <compname> <appnum>\n",
			       argv[0]);
		exit(1);
	}

	/* convert the compname to binary form comp */

	comp = cname_to_comp(argv[1]);
	if (comp == -1)
		exit(1);

	/* make a connection to the chatserver */

	conn = make_contact(comp, (appnum) atoi(argv[2]));
	if (conn < 0) 
		exit(1);

	(void) printf("Chat Connection Established.\n");
	(void) printf(INPUT_PROMPT);
	(void) fflush(stdout);

	/* iterate, reading from local user and then from chatserver */

	while((len = readln(buff, BUFFSIZE)) > 0) {
		buff[len - 1] = '\n';
		(void) send(conn, buff, len, 0);
		
		/* receive and print a line from the chatserver */
		if ((len = recvln(conn, buff, BUFFSIZE)) < 1)
			break;
		(void) printf(RECEIVED_PROMPT);
		(void) fflush(stdout);
		(void) write(STDOUT_FILENO, buff, len);

		(void) printf(INPUT_PROMPT);
		(void) fflush(stdout);
	}

	/* iteration ends when stdin or the connection indicates EOF */

	(void) printf("\nChat Connection Closed.\n");
	(void) send_eof(conn);
	exit(0);
}

