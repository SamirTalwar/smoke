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
BIN_DEBUG := $(OUT_DEBUG)/smoke
OUT_RELEASE := out/build/release
BIN_RELEASE := $(OUT_RELEASE)/smoke

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

$(BIN_RELEASE):
	stack clean
	stack build
	stack install --local-bin-path=$(OUT_RELEASE)

$(BIN_DEBUG): $(CONF) $(SRC)
	$(STACK) install --fast --local-bin-path=$(OUT_DEBUG)

.PHONY: test
test: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) test

.PHONY: bless
bless: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) --bless test

.PHONY: lint
lint: ~/.local/bin/cabal2nix ~/.local/bin/hindent ~/.local/bin/hlint
	$(STACK) exec -- hlint .
	echo Setup.hs $(SRC) | xargs -n1 $(STACK) exec -- hindent --validate
	@ (set -ex; \
		NIX_FILE="$$(mktemp)"; \
		$(STACK) exec -- cabal2nix --shell . > "$$NIX_FILE"; \
		git diff --no-index --exit-code default.nix "$$NIX_FILE" && SUCCESS=true || SUCCESS=false; \
		rm "$$NIX_FILE"; \
		$$SUCCESS)

.PHONY: check
check: test lint

.PHONY: reformat
reformat:
	echo Setup.hs $(SRC) | xargs -n1 $(STACK) exec -- hindent

default.nix: smoke.cabal ~/.local/bin/cabal2nix
	$(STACK) exec -- cabal2nix --shell . > default.nix

~/.local/bin/cabal2nix:
	$(STACK) install cabal2nix

~/.local/bin/ghc-mod:
	$(STACK) install ghc-mod

~/.local/bin/hindent:
	$(STACK) install hindent

~/.local/bin/hlint:
	$(STACK) install hlint
