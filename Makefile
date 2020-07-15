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

CONF := smoke.cabal
NIX_FILES = $(wildcard *.nix nix/*.nix)
SRC_DIR := src
SRC = $(shell find $(SRC_DIR) -name '*.hs')
OUT := out
OUT_BUILD = $(OUT)/build
OUT_NIX := $(OUT_BUILD)/nix
BIN_NIX := $(OUT_NIX)/bin/smoke
OUT_DEBUG := $(OUT_BUILD)/debug
BIN_DEBUG := $(OUT_DEBUG)/smoke
OUT_RELEASE := $(OUT_BUILD)/release
BIN_RELEASE := $(OUT_RELEASE)/smoke

CABAL := cabal --enable-nix

.PHONY: build
build: $(BIN_DEBUG)

.PHONY: dist
dist: clean $(OUT)/smoke-$(OS)

$(OUT)/smoke-$(OS): $(BIN_RELEASE)
	mkdir -p $(OUT)
	cp $(BIN_RELEASE) $(OUT)/smoke-$(OS)

$(BIN_NIX): $(CONF) $(SRC)
	nix-build --out-link $(OUT_NIX)

$(BIN_DEBUG): $(CONF) $(SRC)
	mkdir -p $(OUT_DEBUG)
	$(CABAL) v2-install --installdir=$(OUT_DEBUG) --install-method=copy --overwrite-policy=always

$(BIN_RELEASE): $(CONF) $(SRC)
	mkdir -p $(OUT_RELEASE)
	$(CABAL) v2-install --enable-optimization=2 --installdir=$(OUT_RELEASE) --install-method=copy --overwrite-policy=always

.PHONY: clean
clean:
	$(CABAL) v2-clean
	rm -rf $(OUT_BUILD)

.PHONY: test
test: unit-test spec

.PHONY: unit-test
unit-test: build
	$(CABAL) v2-test

.PHONY: spec
spec: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) spec

.PHONY: bless
bless: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) --bless spec

.PHONY: lint
lint: smoke.cabal
	@ echo >&2 '> hlint'
	@ hlint $(SRC_DIR)
	@ echo >&2 '> ormolu'
	@ ormolu --mode=check $(SRC)
	@ echo >&2 '> nixpkgs-fmt'
	@ nixpkgs-fmt --check $(NIX_FILES)
	@ echo >&2 'Linting succeeded.'

.PHONY: check
check: test lint

.PHONY: reformat
reformat: smoke.cabal
	ormolu --mode=inplace $(SRC)
	nixpkgs-fmt $(NIX_FILES)

.PHONY: freeze
freeze:
	niv update
	# Need to run these in a new Nix shell to make sure changes are picked up.
	nix-shell --pure --run '$(CABAL) v2-update'
	nix-shell --pure --run '$(CABAL) v2-freeze'
