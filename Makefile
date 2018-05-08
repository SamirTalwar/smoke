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

CONF = Setup.hs smoke.cabal stack.yaml
SRC = $(shell find app src -name '*.hs')
OUT_DEBUG := out/build/debug
BIN_DEBUG := $(OUT_DEBUG)/smoke-exe
OUT_RELEASE := out/build/release
BIN_RELEASE := $(OUT_RELEASE)/smoke-exe

ifdef CI
  STACK := stack --no-terminal
else
  STACK := stack
endif

.PHONY: build
build: $(BIN_DEBUG)

.PHONY: dist
dist: out/smoke-$(OS)

out/smoke-$(OS): $(BIN_RELEASE)
	stack clean
	stack build
	stack install --local-bin-path=$(OUT_RELEASE)
	cp $(BIN_RELEASE) out/smoke-$(OS)

$(BIN_DEBUG): $(CONF) $(SRC)
	$(STACK) install --fast --local-bin-path=$(OUT_DEBUG)

.PHONY: test
test: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) test

.PHONY: lint
lint: ~/.local/bin/hlint
	$(STACK) exec -- hlint .

.PHONY: check
check: test lint

.PHONY: reformat
reformat:
	$(STACK) exec -- hindent Setup.hs $(SRC)

.PHONY: dependencies
dependencies: build-dependencies editor-dependencies
	$(STACK) install --only-dependencies

.PHONY: build-dependencies
build-dependencies: ~/.local/bin/hlint

.PHONY: editor-dependencies
editor-dependencies: ~/.local/bin/ghc-mod ~/.local/bin/hindent

default.nix: smoke.cabal
	cabal2nix --shell . > default.nix

~/.local/bin/ghc-mod:
	$(STACK) install ghc-mod

~/.local/bin/hindent:
	$(STACK) install hindent

~/.local/bin/hlint:
	$(STACK) install hlint
