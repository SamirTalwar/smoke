ifeq ($(OS),Windows_NT)
  OS := windows
else
  UNAME_S := $(shell uname -s)
  ifeq ($(UNAME_S),Linux)
    OS := linux
  endif
  ifeq ($(UNAME_S),Darwin)
    OS := macos
  endif
  ifeq ($(OS),)
    $(error "Could not detect the OS.")
  endif
endif

BIN := out/build/smoke-exe

.PHONY: build
build: $(BIN) out/smoke-$(OS)

out/smoke-$(OS):
	cp $(BIN) out/smoke-$(OS)

$(BIN):
	stack build
	stack install --local-bin-path=out/build

.PHONY: test
test: build
	./bin/smoke --command=$(BIN) test

.PHONY: lint
lint: ~/.local/bin/hlint
	stack exec -- hlint .

.PHONY: check
check: test lint

.PHONY: editor-dependencies
editor-dependencies: ~/.local/bin/ghc-mod ~/.local/bin/hindent ~/.local/bin/hlint

~/.local/bin/ghc-mod:
	stack install ghc-mod

~/.local/bin/hindent:
	stack install hindent

~/.local/bin/hlint:
	stack install hlint
