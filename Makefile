# This is a makefile created by Christian Bunk
# Last update 03.07.2011

# Files to be compiled
OBJECTS = $(SRC)/Main.o \
$(SRC)/Index.o \

# program name 
NAME = index

# source directory
SRC = ./src

install: 
	ghc --make $(SRC)/Main.hs $(SRC)/Index.hs -o index

clean:	FORCE
	rm -f $(SRC)/*.o $(SRC)/*.hi 

FORCE:
