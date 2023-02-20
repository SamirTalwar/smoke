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

CONF := package.yaml stack.yaml
NIX_FILES := $(wildcard *.nix nix/*.nix)
SRC_DIR := src
SRC := $(shell find $(SRC_DIR) -name '*.hs')
OUT := out
OUT_BUILD := $(OUT)/build
OUT_NIX := $(OUT_BUILD)/nix
BIN_NIX := $(OUT_NIX)/bin/smoke
OUT_DEBUG := $(OUT_BUILD)/debug
BIN_DEBUG := $(OUT_DEBUG)/smoke
OUT_RELEASE := $(OUT_BUILD)/release
BIN_RELEASE := $(OUT_RELEASE)/smoke

ifdef CI
  STACK := stack --no-terminal
else
  STACK := stack
endif

.PHONY: build
build: $(BIN_DEBUG)

.PHONY: dist
dist: clean $(OUT)/smoke-$(OS)

$(OUT)/smoke-$(OS): $(BIN_RELEASE)
	cp $(BIN_RELEASE) $(OUT)/smoke-$(OS)

$(BIN_NIX): $(CONF) $(SRC)
	nix build --out-link $(OUT_NIX)

$(BIN_DEBUG): $(CONF) $(SRC)
	$(STACK) install --fast --test --no-run-tests --local-bin-path=$(OUT_DEBUG)

$(BIN_RELEASE): $(CONF) $(SRC)
	$(STACK) install --local-bin-path=$(OUT_RELEASE)

.PHONY: clean
clean:
	$(STACK) clean
	rm -rf $(OUT_BUILD)

.PHONY: test
test: unit-test spec

.PHONY: unit-test
unit-test: build
	$(STACK) test --fast

.PHONY: spec
spec: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) spec

.PHONY: spec-nix
spec-nix: $(BIN_NIX)
	$(BIN_NIX) --command=$(BIN_NIX) spec

.PHONY: bless
bless: build
	$(BIN_DEBUG) --command=$(BIN_DEBUG) --bless spec

.PHONY: lint
lint: $(NIX_FILES) smoke.cabal $(SRC)
	@ echo >&2 '> hlint'
	@ hlint $(SRC_DIR)
	@ echo >&2 '> ormolu'
	@ ormolu --mode=check $(SRC)
	@ echo >&2 'nix fmt'
	@ nix fmt -- --check $(NIX_FILES)
	@ echo >&2 'Linting succeeded.'

.PHONY: check
check: test lint

.PHONY: reformat
reformat: $(NIX_FILES) $(SRC)
	ormolu --mode=inplace $(SRC)
	nix fmt -- $(NIX_FILES)

smoke.cabal: $(CONF)
	hpack
	touch $@

.PHONY: update-resolver
update-resolver: update-resolver-in-stack.yaml update-ghc-version

.PHONY: update-resolver-in-stack.yaml
update-resolver-in-stack.yaml:
	sed -i -r "s/^(resolver:) .*/\1 $$(curl -fsSI 'https://www.stackage.org/lts' | grep '^location: ' | sed 's#^location: /##' | dos2unix)/" stack.yaml

.PHONY: update-ghc-version
update-ghc-version: ghc.version update-github-workflow-ghc-version

.PHONY: update-github-workflow-ghc-version
update-github-workflow-ghc-version:
	sed -i -r "s/^  GHC_VERSION: \".*\"$$/  GHC_VERSION: \"$$(cat ghc.version)\"/" .github/workflows/*.yaml

ghc.version: $(CONF)
	curl -fsSL "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/$$(yq -r '.resolver | gsub("[-\\.]"; "/")' stack.yaml).yaml" \
		| yq -r '.resolver.compiler | sub("^ghc-"; "")' \
		> $@
