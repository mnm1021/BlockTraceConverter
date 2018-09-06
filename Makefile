all: converter

converter: converter.c
	gcc -o converter converter.c

clean:
	rm converter
