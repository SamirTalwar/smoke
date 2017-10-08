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

ifdef CI
  STACK := stack --no-terminal
else
  STACK := stack
endif

.PHONY: build
build: $(BIN) out/smoke-$(OS)

out/smoke-$(OS):
	cp $(BIN) out/smoke-$(OS)

$(BIN):
	$(STACK) build
	$(STACK) install --local-bin-path=out/build

.PHONY: test
test: build
	$(BIN) --command=$(BIN) test

.PHONY: lint
lint: ~/.local/bin/hlint
	$(STACK) exec -- hlint .

.PHONY: check
check: test lint

.PHONY: dependencies
dependencies: build-dependencies editor-dependencies
	$(STACK) install --only-dependencies

.PHONY: build-dependencies
build-dependencies: ~/.local/bin/hlint

.PHONY: editor-dependencies
editor-dependencies: ~/.local/bin/ghc-mod ~/.local/bin/hindent

~/.local/bin/ghc-mod:
	$(STACK) install ghc-mod

~/.local/bin/hindent:
	$(STACK) install hindent

~/.local/bin/hlint:
	$(STACK) install hlint
