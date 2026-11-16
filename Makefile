# Makefile for Brainfuck64

ifeq ($(OS),)
	ifeq ($(shell uname -s),Linux)
		OS := Linux
	endif
	ifeq ($(shell uname -s),Darwin)
		OS := macOS
	endif
endif

ifeq ($(OS),)
	OS := Windows_NT
endif

ifeq ($(filter $(OS),Linux macOS Windows_NT),)
	$(error OS must be set to 'Linux', 'macOS', or 'Windows_NT'.)
endif

$(info OS: $(OS))

ifeq ($(OS),Windows_NT)
	RM := del /Q
	EXTENSION := .exe
	FIXPATH = $(subst /,\,$1)
else
	RM := rm -fr
	EXTENSION :=
	FIXPATH = $1
endif

SEARCH_PATHS := $(QB64PE_PATH) . ../qb64pe ../QB64pe ../QB64PE

QB64PE_PATH_FOUND = $(firstword $(foreach dir,$(SEARCH_PATHS),$(if $(wildcard $(dir)/qb64pe$(EXTENSION)),$(dir),)))

ifneq ($(QB64PE_PATH_FOUND),)
	QB64PE ?= $(QB64PE_PATH_FOUND)/qb64pe$(EXTENSION)
else
	QB64PE ?= qb64pe$(EXTENSION)
endif

$(info Using QB64PE from: $(QB64PE))

QB64PE_FLAGS := -x -w -e

ifeq ($(OS),Windows_NT)
	QB64PE_FLAGS += -f:OptimizeCppProgram=true
endif

APP_EXECUTABLE := Brainfuck64$(EXTENSION)

SOURCES := source/Brainfuck64.bas
INCLUDES := $(wildcard source/*.bi)
RESOURCES := source/Brainfuck64.ico

.PHONY: all test clean

all: $(APP_EXECUTABLE)

$(APP_EXECUTABLE): $(SOURCES) $(INCLUDES) $(RESOURCES)
	$(QB64PE) $(QB64PE_FLAGS) $< -o $@

test: all
ifeq ($(OS),Windows_NT)
	powershell.exe -NoProfile -ExecutionPolicy Bypass -File tests/tests.ps1
else
	chmod +x tests/tests.sh
	./tests/tests.sh
endif

clean:
ifeq ($(OS),Windows_NT)
	@if exist "$(APP_EXECUTABLE)" del /q /f "$(APP_EXECUTABLE)"
	@if exist "source\$(APP_EXECUTABLE)" del /q /f "source\$(APP_EXECUTABLE)"
else
	rm -rf $(APP_EXECUTABLE) source/$(APP_EXECUTABLE)
endif
