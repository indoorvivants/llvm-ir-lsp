RELEASE_MODE ?= debug


all: lsp test

clean:
	rm -rf bin/*
	scala-cli clean .
	scala-cli clean build/

bin/LLVM_LanguageServer:
	mkdir -p bin
	scala-cli package *.scala -M LLVM_Lsp --native-mode $(RELEASE_MODE) --native  -f -o bin/LLVM_LanguageServer$(BUILD_SUFFIX)

test:
	scala-cli test *.scala

lsp: bin/LLVM_LanguageServer
