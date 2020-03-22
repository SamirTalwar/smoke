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
SRC_DIRS := app src test
SRC = $(shell find $(SRC_DIRS) -name '*.hs')
OUT := out
OUT_BUILD = $(OUT)/build
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

$(BIN_RELEASE): $(CONF) $(SRC)
	mkdir -p $(OUT_RELEASE)
	$(CABAL) v2-install --enable-optimization=2 --installdir=$(OUT_RELEASE) --install-method=copy --overwrite-policy=always

$(BIN_DEBUG): $(CONF) $(SRC)
	mkdir -p $(OUT_DEBUG)
	$(CABAL) v2-install --installdir=$(OUT_DEBUG) --install-method=copy --overwrite-policy=always

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
	@ hlint $(SRC_DIRS)
	@ echo >&2 '> ormolu'
	@ ormolu --mode=check $(SRC)
	@ echo >&2 '> cabal2nix'
	@ ( \
		set -e; \
		NIX_FILE="$$(mktemp)"; \
		trap 'rm -r $$NIX_FILE' EXIT; \
		cabal2nix . > "$$NIX_FILE"; \
		nixpkgs-fmt "$$NIX_FILE"; \
		git diff --no-index --exit-code app.nix "$$NIX_FILE" \
	)
	@ echo >&2 'Linting succeeded.'

.PHONY: check
check: test lint

.PHONY: reformat
reformat: smoke.cabal
	ormolu --mode=inplace $(SRC)
	nixpkgs-fmt *.nix

cabal.project.freeze: smoke.cabal app.nix
	rm -f $@
	$(CABAL) v2-freeze

app.nix: smoke.cabal
	cabal2nix . > $@
	nixpkgs-fmt $@
