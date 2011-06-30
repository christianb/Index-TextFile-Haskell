# This is a makefile created by Christian Bunk
# Last update 02.05.2011

# Define C Compiler
CC = ghc
# Define standard flags
CFLAGS = -Wall -Wextra -g -DLexic_Compare -DTwo_Options

# Files to be compiled
OBJECTS = $(SRC)/Main.o \
$(SRC)/Index.o \

# program name 
NAME = index

# bin directory
BIN = ./bin

# source directory
SRC = ./src

# target (merge with bin and name)
TARGET = $(BIN)/$(NAME)

install: 
	ghc --make $(SRC)/Main.hs $(SRC)/Index.hs -o index

clean:	FORCE
	rm -f $(SRC)/*.o $(SRC)/*.hi 

FORCE:
