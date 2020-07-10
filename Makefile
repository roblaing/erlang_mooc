# A simple Makefile, must use tabs, not spaces, to indent
ERLC_FLAGS=+debug_info
APP_NAME=futurelearn
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard include/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
all: $(OBJECTS)
ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<
clean:
	-rm $(OBJECTS)
.PHONY: test
test:
	ct_run -pa ebin -dir test -logdir test
.PHONY: doc
doc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '[{dir, "doc"}, {subpackages, true}]'

