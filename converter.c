#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Simple converter for the result of FIO latency-log.
 */
int main(int argc, char **argv)
{
	int timestamp, type, size, lat;
	FILE *fp_in;
	FILE *fp_read, *fp_write;
	char buf[500];

	if (argc != 4)
	{
		printf ("usage: ./converter [input filename] [output read filename] [output write filename]");
		return -1;
	}

	fp_in = fopen (argv[1], "r");
	fp_read = fopen (argv[2], "w");
	fp_write = fopen (argv[3], "w");

	while (fscanf (fp_in, "%d,%d\n", &type, &lat) == 2)
	{
		if (type == 0)
			fprintf (fp_read, "%d\n", lat);
		else
			fprintf (fp_write, "%d\n", lat);
	}

	fclose (fp_in);
	fclose (fp_read);
	fclose (fp_write);

	return 0;
}
