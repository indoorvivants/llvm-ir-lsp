RELEASE_MODE ?= debug
BUILD_SUFFIX ?= 
all: lsp test

clean:
	rm -rf bin/*
	scala-cli clean .

bin/LLVM_LanguageServer:
	mkdir -p bin
	scala-cli package . -M LLVM_Lsp --native-mode $(RELEASE_MODE) --native  -f -o bin/LLVM_LanguageServer$(BUILD_SUFFIX)

test:
	scala-cli test .

lsp: bin/LLVM_LanguageServer
