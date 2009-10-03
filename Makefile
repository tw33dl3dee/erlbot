#

BIN = ebin
SRC = src
ERLC = erlc
ERLC_FLAGS = -I src -pa ebin
#+nowarn_unused_function
BEAMS = $(patsubst $(SRC)/%.erl,$(BIN)/%.beam, $(wildcard $(SRC)/*.erl))
INCLUDES = $(wildcard $(SRC)/*.hrl)

.PHONY: clean all
.SUFFIXES: .beam .erl

all: $(BEAMS)

$(BIN)/bhv*.beam: $(BIN)/irc_behaviour.beam

# Dirty but should work
$(BIN)/%.beam: $(SRC)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -o $(BIN) "$<"

clean:
	$(RM) $(BEAMS)
