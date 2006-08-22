//Bill Mill lab 2.27.01

#include <stdlib.h>
#include <stdio.h>

//IP Type Constants
#define IPT_ICMP 		1 	/* protocol type for ICMP packets */
#define IPT_TCP 		6 	/* protocol type for TCP packets */
#define IPT_UDP 		17 	/* protocol type for UDP packets */

//ARP Type Constants
#define EPT_IP			0x0800 	/* Internet Protocol */ 
#define EPT_ARP			0x0806 	/* Address Resolution Protocol */

//TCP Type Constants
#define PORT_FTP		21 	/* reserved port for FTP server */
#define PORT_SMTP		25  /* reserved port for mail server */
#define PORT_TELNET 	23 	/* reserved port for Telnet server */
#define PORT_HTTP 		80 	/* reserved port for HTTP server */


//Structure to contain Snoop headers
struct RecHeader {
unsigned long framelen;  		// frame length as received
unsigned long tracelen;    	// frame length as saved
unsigned long reclen;   		// length of entire record
unsigned long pad;  			// ignored
struct timeval {
 unsigned long tv_sec;    		// reception time stamp in seconds  
						// since 1/1/70
 unsigned long tv_usec;		// and microseconds.
} TimeStamp;
};

//Structure to contain ethernet headers
struct Packet {
unsigned char dest[6]; 	// destination
unsigned char source[6]; 	// source
unsigned short type; 			// protocol type
};

//Structure to contain IP headers
struct IP 
{
	unsigned char verlen;		/* IP version and header length */
	unsigned char serviceType;	/* type of service */
	unsigned short length;		/* total packet length in bytes */
	unsigned short id;			/* datagram id */
	unsigned short fragoffset;	/* fragment offset */
	unsigned char lifetime;		/* time to live, in gateway hops */
	unsigned char protocolType;	/* IP protocol */
	unsigned short checksum;	/* header checksum */
	unsigned char ipSource[4];	/* IP address of source */
	unsigned char ipDest[4];	/* IP address of target */
};



//Structure to contain ARP headers
struct ARP 
{
	unsigned short hwType;		/* hardware type */
	unsigned short prType;		/* protocol type */
	unsigned char hwLen;		/* hardware address length */
	unsigned char prLen;		/* protocol address length */
	unsigned short operation;	/* arp operation */
	unsigned char macSource[6];	/* sender's MAC address */
	unsigned char pSource[4];	/* sender's protocol address */
	unsigned char macDest [6];	/* target's MAC address */
	unsigned char pDest[4];		/* target's protocol address */
};

//Structure to contain TCP headers
struct TCP
{
	unsigned short srcPort;				/*Source port num*/
	unsigned short destPort;			/*Destination port num*/
	unsigned long sequenceNum;			/*TCP sequence number*/
	unsigned long ackNum;				/*Acknowledgement number*/
	unsigned char len;					/*first 4 bits = length in # of 4-byte words*/
	unsigned char flags;				/*last 6 bits = flags*/
	unsigned short window;				/*TCP window size*/
	unsigned short checksum;			/*header checksum*/
	unsigned short urgPtr;				/*pointer to urgent data, if any*/
};

struct UDP
{
	unsigned short srcPort;		/*Source Port*/
	unsigned short destPort;	/*Destination Port*/
	unsigned short length;		/*Packet Length, including header, in octets*/
	unsigned short checksum;	/*checksum*/
};


void BigEndToLittleEnd32(unsigned long *word)
// Reverse the order of the 4 bytes in the word
{
	unsigned char * helper = (unsigned char *) word;
	unsigned char temp; 
	temp = *helper;			// swap first and fourth byte
	*helper = *(helper+3);
	*(helper+3) = temp;
	temp = *(helper+1);		// swap second and third byte
	*(helper+1) = *(helper+2);
	*(helper+2) = temp;
}

void BigEndToLittleEnd16(unsigned short *word)
// Reverse the order of a 2 byte word
{
	unsigned char *helper = (unsigned char *) word;
	unsigned char temp = *helper;
	*helper = *(helper + 1);
	*(helper+1) = temp;
}

void printEther(FILE *outFile, struct Packet *pack, int recordCounter, int packetLen)
// PRE: pass a reference to an open file and a Packet structure, along with what number
//		record this is and the length of the record
//POST: will output the given info in the correct format
{
	int i;

	fprintf(outFile, "ETHER:-----Ether Header-----\nETHER:\n");
	fprintf(outFile, "ETHER: Packet number %i\n", recordCounter);
	fprintf(outFile, "ETHER: Packet Size = %i bytes\n", packetLen);

	fprintf(outFile, "ETHER: Destination Address = ");
	for(i = 0; i < 6; i++)
	{
		if(i!=5)
			fprintf(outFile, "%.2x:", pack->dest[i]);
		else
			fprintf(outFile, "%.2x", pack->dest[i]);
	}

	fprintf(outFile, "\nETHER: Source Address = ");
	for(i = 0; i < 6; i++)
	{
		if(i!=5)
			fprintf(outFile, "%.2x:", pack->source[i]);
		else
			fprintf(outFile, "%.2x", pack->source[i]);
	}

	fprintf(outFile, "\nETHER: Frame Type = %#.4x", pack->type);

	switch (pack->type)
	{
	case EPT_IP:
		fprintf(outFile, "(IP)\n");
		break;
	case EPT_ARP:
		fprintf(outFile, "(ARP)\n");
		break;
	default:
		fprintf(outFile, "(unknown)\n");
	}

	fprintf(outFile, "ETHER:\n");
}

void printIP(FILE *outFile, struct IP *ip)
// PRE: pass a reference to an open file and an IP structure
//POST: will output the given info in the correct format
{
	int i, fragFlag, lastFlag;

	/*read the flags from the IP header*/
	if(fragFlag = ip -> fragoffset & 16384) /*16384 = 2^14*/
		fragFlag = 1;
	else
		fragFlag = 0;
	if(lastFlag = ip -> fragoffset & 8192) /*8192 = 2^13*/
		lastFlag = 1;
	else
		lastFlag = 0;

	/*output IP info*/
	fprintf(outFile, "IP: -----IP Header-----\nIP:\n");
	fprintf(outFile, "IP: Version = %i\n", ip->verlen / 16);
	fprintf(outFile, "IP: Header Length = %i bytes\n", (ip->verlen % 16) * 4);
	fprintf(outFile, "IP: Total Length = %u bytes\n", ip->length);
	fprintf(outFile, "IP: Identification = %u bytes\n", ip->id);
	fprintf(outFile, "IP: Flags:\n");
	fprintf(outFile, "IP: .%i.. .... = ", fragFlag);
	if (fragFlag)
		fprintf(outFile, "may fragment\n");
	else
		fprintf(outFile, "may not fragment\n");
	fprintf(outFile, "IP: ..%i. .... = ", lastFlag);
	if(lastFlag)
		fprintf(outFile, "more fragments to arrive\n");
	else
		fprintf(outFile, "final fragment\n");
	/*the binary and masks the first 4 bits of the frag offset (the flags)*/
	fprintf(outFile, "IP: fragment offset = %i bytes\n", ip->fragoffset & 4095);
	fprintf(outFile, "IP: Time to Live = %u seconds/hops\n", ip->lifetime);
	fprintf(outFile, "IP: Protocol = %u ", ip->protocolType);
	switch (ip->protocolType)
	{
	case IPT_ICMP:
		fprintf(outFile, "(ICMP)\n");
		break;
	case IPT_TCP:
		fprintf(outFile, "(TCP)\n");
		break;
	case IPT_UDP:
		fprintf(outFile, "(UDP)\n");
		break;
	default:
		fprintf(outFile, "(Unknown type)\n");
	}
	fprintf(outFile, "IP: Source IP Address = ");
	for(i = 0; i < 4; i++)
	{
		if(i != 3)
			fprintf(outFile, "%i.", ip->ipSource[i]);
		else
			fprintf(outFile, "%i", ip->ipSource[i]);
	}
	fprintf(outFile, "\nIP: Destination IP Address = ");
	for(i = 0; i < 4; i++)
	{
		if(i != 3)
			fprintf(outFile, "%i.", ip->ipDest[i]);
		else
			fprintf(outFile, "%i", ip->ipDest[i]);
	}
	fprintf(outFile, "\n");
}

void printARP(FILE *outFile, struct ARP *arp)
// PRE: pass a reference to an open file and a ARP structure
//POST: will output the given info in the correct format
{
	int i;
	int isAllZeros = 1;

	fprintf(outFile, "ARP:-----ARP Frame-----\nARP:\n");
	fprintf(outFile, "ARP: Hardware Type = %u\n", arp->hwType);
	fprintf(outFile, "ARP: Protocol Type = %#.4x\n", arp->prType);
	fprintf(outFile, "ARP: Length of Hardware Address = %u bytes\n", arp->hwLen);
	fprintf(outFile, "ARP: Length of Protocol Address = %u bytes\n", arp->prLen);
	fprintf(outFile, "ARP: Operation %u\n", arp->operation);
	fprintf(outFile, "ARP: Sender Hardware Address = ");
	for(i = 0; i < 6; i++)
	{
		if(i!=5)
			fprintf(outFile, "%.2x:", arp->macSource[i]);
		else
			fprintf(outFile, "%.2x", arp->macSource[i]);
	}
	fprintf(outFile, "\nARP: Sender Protocol Address = ");
	for(i = 0; i < 4; i++)
	{
		if(i != 3)
			fprintf(outFile, "%i.", arp->pSource[i]);
		else
			fprintf(outFile, "%i", arp->pSource[i]);
	}
	fprintf(outFile, "\nARP: Target Hardware Address = ");
	for(i = 0; i < 6 && isAllZeros != 0; i++)
	{
		if(arp->macDest[i] == 0x00)
			isAllZeros = 1;
		else
			isAllZeros = 0;
	}
	if(!isAllZeros)
		for(i = 0; i < 6; i++)
		{
			if(i != 5)
				fprintf(outFile, "%.2x:", arp->macDest[i]);
			else
				fprintf(outFile, "%.2x", arp->macDest[i]);
		}
	else
		fprintf(outFile, "?");
	fprintf(outFile, "\nARP: Target Protocol Address = ");
	for(i = 0; i < 4; i++)
	{
		if(i != 3)
			fprintf(outFile, "%i.", arp->pDest[i]);
		else
			fprintf(outFile, "%i", arp->pDest[i]);
	}
	fprintf(outFile, "\n\n");
}

void printTCP(FILE *outFile, struct TCP *tcp)
// PRE: pass a reference to an open file and a ARP structure
//POST: will output the given info in the correct format
{
	int urgFlag, ackFlag, pushFlag, resetFlag, syncFlag, finFlag;
	/*read flags*/
	if(urgFlag = tcp->flags & 32)
		urgFlag = 1;
	else
		urgFlag = 0;
	if(ackFlag = tcp->flags & 16)
		ackFlag = 1;
	else
		urgFlag = 0;
	if(pushFlag = tcp->flags & 8)
		pushFlag = 1;
	else
		pushFlag = 0;
	if(resetFlag = tcp->flags & 4)
		resetFlag = 1;
	else
		resetFlag = 0;
	if(syncFlag = tcp->flags & 2)
		syncFlag = 1;
	else
		syncFlag = 0;
	if(finFlag = tcp->flags & 1)
		finFlag = 1;
	else
		finFlag = 0;

	/*output information*/
	fprintf(outFile, "TCP: ----- TCP Header -----\n");
	fprintf(outFile, "TCP:\nTCP: Source Port = %u\n", tcp->srcPort);
	fprintf(outFile, "TCP: Destination Port = %u", tcp->destPort);
	switch(tcp->destPort)
	{
	case PORT_FTP:
		fprintf(outFile, " (FTP)\n");
		break;
	case PORT_SMTP:
		fprintf(outFile, " (SMTP)\n");
		break;
	case PORT_TELNET:
		fprintf(outFile, " (TELNET)\n");
		break;
	case PORT_HTTP:
		fprintf(outFile, " (HTTP)\n");
		break;
	default:
		fprintf(outFile, " (UNKNOWN)\n");
		break;
	}
	fprintf(outFile, "TCP: Sequence Number = %u\n", tcp->sequenceNum);
	fprintf(outFile, "TCP: Acknowledgement Number = %u\n", tcp->sequenceNum);
	fprintf(outFile, "TCP: Data Offset = %u\n", tcp->len / 16);
	fprintf(outFile, "TCP: Flags: %#x\n", tcp->flags);
	fprintf(outFile, "TCP: ..%i. .... ", urgFlag);
	
	/*print flags*/
	if(urgFlag)
		fprintf(outFile, "= Urgent Pointer\n");
	else
		fprintf(outFile, "= No Urgent Pointer\n");

	fprintf(outFile, "TCP: ...%i .... ", ackFlag);
	if(ackFlag)
		fprintf(outFile, "= Acknowledgement\n");
	else
		fprintf(outFile, "= No Acknowledgement\n");

	fprintf(outFile, "TCP: .... %i... ", pushFlag);
	if(pushFlag)
		fprintf(outFile, "= Push\n");
	else
		fprintf(outFile, "= No Push\n");

	fprintf(outFile, "TCP: .... .%i.. ", resetFlag);
	if(resetFlag)
		fprintf(outFile, "= Reset\n");
	else
		fprintf(outFile, "= No Reset\n");

	fprintf(outFile, "TCP: .... ..%i. ", syncFlag);
	if(syncFlag)
		fprintf(outFile, "= Syn\n");
	else
		fprintf(outFile, "= No Syn\n");

	fprintf(outFile, "TCP: .... ...%i ", finFlag);
	if(finFlag)
		fprintf(outFile, "= Fin\n");
	else
		fprintf(outFile, "= No Fin\n");


	fprintf(outFile, "TCP: Window = %u\n", tcp->window);
	fprintf(outFile, "TCP: Checksum = %#.4X\n", tcp->checksum);
}

void printUDP(FILE *outFile, struct UDP *udp)
{
	fprintf(outFile, "UDP: ----- UDP Header -----\nUDP:\n");
	fprintf(outFile, "UDP: Source port = %u\n", udp->srcPort);
	fprintf(outFile, "UDP: Destination port = %u\n", udp->destPort);
	fprintf(outFile, "UDP: Length = %u\n", udp->length);
	fprintf(outFile, "UDP: Checksum = %#.4x\n", udp->checksum);
}

int main(int argc, char *argv[])
{
	/*variable declarations*/
	FILE *inFile;
	FILE *outFile;
	struct RecHeader headerBuf;
	struct Packet pack;
	struct ARP arp;
	struct IP ip;
	struct TCP tcp;
	struct UDP udp;
	unsigned char packetBuf[64000];
	int n, i = 0;
	int packetLen;
	long recordCounter = 0;
	long sizeTotal = 0;
	int isAllZeros = 1;

	/*open files*/
	inFile = fopen("snoop.dat", "rb");
	if(!inFile)
	{
		printf("There has been an error opening the file snoopIn.dat\n");
		return 0;
	}

	outFile = fopen("snoopOut.dat", "wt");
	if(!outFile)
	{
		printf("There has been an error creating snoopOut.dat\n");
		return 0;
	}

	/*skip the file header*/
	fseek(inFile, 16, SEEK_SET);

	/*read one packet per iteration, extract information*/
	while(n = fread(&headerBuf, sizeof(struct RecHeader), 1, inFile))
	{
		/*Convert the read hex val into LittleEndian*/
		BigEndToLittleEnd32(&headerBuf.reclen);

		/*The packet is size of frame - snoopHeader long*/
		packetLen = headerBuf.reclen - sizeof(struct RecHeader);

		/*Read the ethernet header into pack*/
		fread(&pack, sizeof(struct Packet), 1, inFile);

		/* Convert big to little endian so that future implementations would
		   be able to use the integer*/
		BigEndToLittleEnd16(&pack.type);

		/*read ARP packet if it exists*/
		if(pack.type == 2054) //2054 == 0x0806 (ARP)
		{
			fread(&arp, sizeof(struct ARP), 1, inFile);
			fread(packetBuf, packetLen - (sizeof(struct Packet) + sizeof(struct ARP)), 1, inFile);
			/*switch integers to little endian for future output*/
			BigEndToLittleEnd16(&arp.hwType);
			BigEndToLittleEnd16(&arp.prType);
			BigEndToLittleEnd16(&arp.operation);
		}

		/*read IP header if it exists, then read TCP or UDP header if they exist*/
		else if(pack.type == 2048) //2048 == 0x0800 (IP)
		{
			fread(&ip, sizeof(struct IP), 1, inFile);
			/*switch integers to little endian for output*/
			BigEndToLittleEnd16(&ip.length);
			BigEndToLittleEnd16(&ip.id);
			BigEndToLittleEnd16(&ip.fragoffset);
			BigEndToLittleEnd16(&ip.checksum);

			/* now read TCP header if it exists*/
			if(ip.protocolType == 6)
			{
				fread(&tcp, sizeof(struct TCP), 1, inFile);
				fread(packetBuf, packetLen - (sizeof(struct Packet) + sizeof(struct IP) 
					+ sizeof(struct TCP)), 1, inFile);

				/*switch integers to little endian for output*/
				BigEndToLittleEnd16(&tcp.srcPort);
				BigEndToLittleEnd16(&tcp.destPort);
				BigEndToLittleEnd32(&tcp.sequenceNum);
				BigEndToLittleEnd32(&tcp.ackNum);
				BigEndToLittleEnd16(&tcp.window);
				BigEndToLittleEnd16(&tcp.checksum);
				BigEndToLittleEnd16(&tcp.urgPtr);
			}
			/*and UDP if it exists*/
			else if(ip.protocolType == 17)
			{
				fread(&udp, sizeof(struct UDP), 1, inFile);
				fread(packetBuf, packetLen - (sizeof(struct Packet) + sizeof(struct IP)
					+ sizeof(struct UDP)), 1, inFile);

				/*switch integers to little endian for output*/
				BigEndToLittleEnd16(&udp.srcPort);
				BigEndToLittleEnd16(&udp.destPort);
				BigEndToLittleEnd16(&udp.length);
				BigEndToLittleEnd16(&udp.checksum);
			}
			else /*if it's neither UDP nor TCP*/
			{
				fprintf(outFile, "\n");
				fread(packetBuf, packetLen - (sizeof(struct Packet) + sizeof(struct IP)), 1, inFile);
			}

		}
		
		/*if there are neither ARP nor IP packets, just read remainder into packetBuf*/
		else
			fread(packetBuf, packetLen - sizeof(struct Packet), 1, inFile);

		/*increment counters*/
		recordCounter++;
		sizeTotal += packetLen;
		
		/*output ethernet info*/
		printEther(outFile, &pack, recordCounter, packetLen);

		/*output ARP info if exists*/
		if(pack.type == 2054)
			printARP(outFile, &arp);

		/*output IP info if exists*/
		else if(pack.type == 2048)
		{
			printIP(outFile, &ip);
			/*and TCP data if exists*/
			if(ip.protocolType == 6)
				printTCP(outFile, &tcp);
			else if(ip.protocolType == 17)
				printUDP(outFile, &udp);
			fprintf(outFile, "\n");
		}

		else
			fprintf(outFile, "\n");

	}//while n = fread...

	/*close files*/
	fclose(inFile);
	fclose(outFile);

	return 0;
}