/* chatserver.c */

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
 * Program: chatserver
 * Purpose: wait for a connection from a chatclient & allow users to chat
 * Usage:   chatserver <appnum>
 *
 *-----------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
	connection	conn;
	int		len;
	char		buff[BUFFSIZE];

	if (argc != 2) {
		(void) fprintf(stderr, "usage: %s <appnum>\n", argv[0]);
		exit(1);
	}

	(void) printf("Chat Server Waiting For Connection.\n");

	/* wait for a connection from a chatclient */

	conn = await_contact((appnum) atoi(argv[1]));
	if (conn < 0)
		exit(1);
	
	(void) printf("Chat Connection Established.\n");
	
	/* iterate, reading from the client and the local user */

	while((len = recvln(conn, buff, BUFFSIZE)) > 0) {
		(void) printf(RECEIVED_PROMPT);
		(void) fflush(stdout);
		(void) write(STDOUT_FILENO, buff, len);
		
		/* send a line to the chatclient */

		(void) printf(INPUT_PROMPT);
		(void) fflush(stdout);
		if ((len = readln(buff, BUFFSIZE)) < 1)
			break;
		buff[len - 1] = '\n';
		(void) send(conn, buff, len, 0);
	}

	/* iteration ends when EOF found on stdin or chat connection */

	(void) send_eof(conn);
	(void) printf("\nChat Connection Closed.\n\n");
	return 0;
}

