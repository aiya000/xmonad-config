all: full_clean

replace:
	./replace.sh

full_clean:
	rm -rf .stack-work
