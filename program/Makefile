TARGET = compiler
TCHECK = Typechecker
SRC_DIR = ./src
GRAMMAR_DIR = Latte
TARGET_NAME = ltc

all: $(SRC_DIR)/$(TARGET).hs $(SRC_DIR)/$(TCHECK).hs
	ghc -i$(SRC_DIR) $(SRC_DIR)/$(TARGET).hs -o ./$(TARGET_NAME)

clean:
	$(RM) $(TARGET_NAME) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi
