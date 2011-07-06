# This is a makefile created by Christian Bunk
# Last update 03.07.2011

# Files to be compiled
OBJECTS = $(SRC)/Main.hs \
$(SRC)/Index.hs \

# program name 
NAME = index

# source directory
SRC = ./src

install: 
	ghc --make $(SRC)/Main.hs $(SRC)/Index.hs $(SRC)/Parser.hs $(SRC)/Cmdlineargs.hs -o index

clean:	FORCE
	rm -f $(SRC)/*.o $(SRC)/*.hi 

FORCE:
