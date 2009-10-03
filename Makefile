#

BIN = ebin
BHVS = irc_behaviour
SRC = src
INC = include
ERLC = erlc
ERLC_FLAGS = -I $(INC) -pa $(BIN)
#+nowarn_unused_function
# Behaviours go first
BEAMS = $(patsubst %,$(BIN)/%.beam, $(BHVS)) $(patsubst $(SRC)/%.erl,$(BIN)/%.beam, $(wildcard $(SRC)/*.erl))
INCLUDES = $(wildcard $(INC)/*.hrl)

.PHONY: clean all
.SUFFIXES: .beam .erl

all: $(BEAMS)

# Dirty but should work
$(BIN)/%.beam: $(SRC)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -o $(BIN) "$<"

clean:
	$(RM) $(BEAMS)
