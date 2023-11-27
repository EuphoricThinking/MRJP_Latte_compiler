TARGET = typechecker
SRC_DIR = ./src

all: $(TARGET).hs
	cd $(SRC_DIR)
	ghc $(TARGET).hs -o ./..

clean:
	$(RM) *.o *.hi $(TARGET) src/Latte/*.o src/Latte/*.hi


