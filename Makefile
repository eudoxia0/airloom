BIN := airloom
.PHONY: install clean

$(BIN):
	stack build --copy-bins --local-bin-path=.
	mv airloom-exe $(BIN)

install: $(BIN)
	install -m 755 $(BIN) /usr/local/bin/$(BIN)

uninstall:
	sudo rm /usr/local/bin/$(BIN)

clean:
	rm $(BIN)