# Compiler and flags
CXX := g++
CXXFLAGS := -Wall -Wextra -Iinclude
# CRITICAL! Don't forget -g and remove optimization
CXXFLAGS +=  -g -O0
# Directories
SRC_DIR := src
OBJ_DIR := obj

# Files
TARGET := math_toolkit
SRC := $(wildcard $(SRC_DIR)/*.cpp)
OBJ := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRC))

# Default build
all: $(TARGET)

# Link target (binary in parent directory)
$(TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Compile rule
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Cleanup
clean:
	rm -rf $(OBJ_DIR) $(TARGET)

# Run
run: all
	./$(TARGET)
