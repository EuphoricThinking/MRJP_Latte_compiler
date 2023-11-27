TARGET = typechecker
SRC_DIR = ./src
GRAMMAR_DIR = Latte

all: $(SRC_DIR)/$(TARGET).hs
	ghc -i$(SRC_DIR) $(SRC_DIR)/$(TARGET).hs -o ./$(TARGET)

clean:
	$(RM) $(TARGET) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi