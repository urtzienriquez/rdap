CC ?= cc
PREFIX ?= $(HOME)/.local

R_HOME := $(shell R RHOME)
R_CPPFLAGS := $(shell R CMD config --cppflags)
R_LDFLAGS := $(shell R CMD config --ldflags)

CFLAGS ?= -O2 -g
CFLAGS += -std=gnu11 -Wall -Wextra -Wno-unused-parameter -Wno-cast-function-type -pthread
CPPFLAGS += $(R_CPPFLAGS) -DRDAP_R_HOME='"$(R_HOME)"' -DRDAP_R_FILE='"$(abspath src/rdap.R)"'
LDLIBS += $(R_LDFLAGS) -Wl,-rpath,$(R_HOME)/lib -pthread

SRC := $(wildcard src/*.c)
OBJ := $(SRC:src/%.c=build/%.o)

build/rdap: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) $(LDLIBS)

build/%.o: src/%.c $(wildcard src/*.h) | build
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

build/rhost.o: src/rdap.R # (built in)

build:
	mkdir -p build

# for clangd
compile_flags.txt:
	printf '%s\n' -std=gnu11 -Wall -Wextra $(R_CPPFLAGS) '-DRDAP_R_FILE="$(abspath src/rdap.R)"' > $@

install: build/rdap
	install -Dm755 build/rdap $(PREFIX)/bin/rdap

test: build/rdap
	python3 tests/run.py

clean:
	rm -rf build

.PHONY: install test clean compile_flags.txt
