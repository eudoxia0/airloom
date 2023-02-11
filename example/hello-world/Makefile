SRC    := hello.c
FRAGS  := frags.json
DOCS   := hello.md
OUTPUT := output.md

.PHONY: all
all: $(OUTPUT)
	diff -q output.md expected.md

$(FRAGS): $(SRC)
	stack run -- lift $(SRC) -o $(FRAGS)

$(OUTPUT): $(FRAGS) $(DOCS)
	stack run -- weave $(DOCS) -f $(FRAGS) -o $(OUTPUT)

clean:
	rm $(FRAGS)
	rm $(OUTPUT)