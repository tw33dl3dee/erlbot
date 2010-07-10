#

BIN = ebin
BHVS = utf8 irc_behaviour test_pt
SRC = src
INC = include
ERLC = erlc
ERLC_FLAGS = -I $(INC) -pa $(BIN) '+{warn_format, 2}'
#+nowarn_unused_function
DIALYZER=dialyzer
# Behaviours go first
BEAMS = $(patsubst %,$(BIN)/%.beam, $(BHVS)) $(patsubst $(SRC)/%.erl,$(BIN)/%.beam, $(wildcard $(SRC)/*.erl))
INCLUDES = $(wildcard $(INC)/*.hrl)

.PHONY: clean all
.SUFFIXES: .beam .erl

all: $(BEAMS)

# Dirty but should work
$(BIN)/%.beam: $(SRC)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_FLAGS) -o $(BIN) "$<"

dialyzer:
	$(DIALYZER) --src -I $(INC) -pa $(BIN) -c $(SRC)

clean:
	$(RM) $(BEAMS)
