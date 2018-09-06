all: spliter

spliter: spliter.c
	gcc -o spliter spliter.c

clean:
	rm spliter
