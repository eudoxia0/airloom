BIN := airloom

$(BIN):
	stack build --copy-bins --local-bin-path=.
	mv airloom-exe $(BIN)

clean:
	rm $(BIN)