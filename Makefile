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

CONF = package.yaml stack.yaml
SRC = $(shell find app src test -name '*.hs')
OUT := out
OUT_BUILD = $(OUT)/build
OUT_DEBUG := $(OUT_BUILD)/debug
BIN_DEBUG := $(OUT_DEBUG)/smoke
OUT_RELEASE := $(OUT_BUILD)/release
BIN_RELEASE := $(OUT_RELEASE)/smoke

ifdef CI
  STACK := stack --no-terminal --nix
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
	$(STACK) build
	$(STACK) install --local-bin-path=$(OUT_RELEASE)

$(BIN_DEBUG): $(CONF) $(SRC)
	$(STACK) install --fast --test --no-run-tests --local-bin-path=$(OUT_DEBUG)

app.nix: smoke.cabal
	cabal2nix . > $@
	nixpkgs-fmt $@

.PHONY: clean
clean:
	$(STACK) clean
	rm -rf $(OUT_BUILD)

.PHONY: test
test: unit-test spec

.PHONY: unit-test
unit-test: build
	$(STACK) test

.PHONY: spec
spec: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) spec

.PHONY: bless
bless: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) --bless spec

.PHONY: lint
lint: smoke.cabal
	@ echo >&2 '> hlint'
	@ $(STACK) exec -- hlint .
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

smoke.cabal: $(CONF)
	$(STACK) install --only-dependencies --test --no-run-tests
	touch $@
