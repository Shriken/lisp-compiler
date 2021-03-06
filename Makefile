BIN_FILE = driver

all: $(BIN_FILE)
$(BIN_FILE): test.s driver.c
	gcc test.s driver.c -o $(BIN_FILE)

test.s: compiler.scm
	csc compiler.scm
	./compiler > test.s

.PHONY: run
run: $(BIN_FILE)
	./$(BIN_FILE)

.PHONY: clean
clean:
	rm -f test.s
	rm -f compiler
	rm -f $(BIN_FILE)
