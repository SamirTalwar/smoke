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

CONF = stack.yaml
SRC = $(shell find app src -name '*.hs')
OUT := out
OUT_BUILD = $(OUT)/build
OUT_DEBUG := $(OUT_BUILD)/debug
BIN_DEBUG := $(OUT_DEBUG)/smoke
OUT_RELEASE := $(OUT_BUILD)/release
BIN_RELEASE := $(OUT_RELEASE)/smoke
TOOLS_BIN_DIR := .stack-work/tools

ifdef CI
  STACK := stack --no-terminal
else
  STACK := stack
endif

.PHONY: build
build: $(BIN_DEBUG)

.PHONY: dist
dist: $(OUT)/smoke-$(OS)

$(OUT)/smoke-$(OS): $(BIN_RELEASE)
	cp $(BIN_RELEASE) $(OUT)/smoke-$(OS)

$(BIN_RELEASE): clean
	stack build
	stack install --local-bin-path=$(OUT_RELEASE)

$(BIN_DEBUG): $(CONF) $(SRC)
	$(STACK) install --fast --local-bin-path=$(OUT_DEBUG)

.PHONY: clean
clean:
	stack clean
	rm -rf $(OUT_BUILD)

.PHONY: test
test: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) test

.PHONY: bless
bless: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) --bless test

.PHONY: lint
lint: tools
	$(TOOLS_BIN_DIR)/hlint .
	echo $(SRC) | xargs -n1 $(TOOLS_BIN_DIR)/hindent --validate
	@ (set -ex; \
		NIX_FILE="$$(mktemp)"; \
		$(TOOLS_BIN_DIR)/cabal2nix --shell . > "$$NIX_FILE"; \
		git diff --no-index --exit-code default.nix "$$NIX_FILE" && SUCCESS=true || SUCCESS=false; \
		rm "$$NIX_FILE"; \
		$$SUCCESS)

.PHONY: check
check: test lint

.PHONY: reformat
reformat: $(TOOLS_BIN_DIR)/hindent
	echo $(SRC) | xargs -n1 $(TOOLS_BIN_DIR)/hindent

default.nix: smoke.cabal $(TOOLS_BIN_DIR)/cabal2nix
	$(TOOLS_BIN_DIR)/cabal2nix --shell . > default.nix

.PHONY: tools
tools: $(TOOLS_BIN_DIR)/cabal2nix $(TOOLS_BIN_DIR)/hindent $(TOOLS_BIN_DIR)/hlint

$(TOOLS_BIN_DIR)/cabal2nix:
	$(STACK) install --local-bin-path=$(TOOLS_BIN_DIR) cabal2nix

$(TOOLS_BIN_DIR)/hindent:
	$(STACK) install --local-bin-path=$(TOOLS_BIN_DIR) hindent

$(TOOLS_BIN_DIR)/hlint:
	$(STACK) install --local-bin-path=$(TOOLS_BIN_DIR) hlint
