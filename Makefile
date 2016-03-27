.PHONY: build
build:
	stack build

.PHONY: test
test: build
	./bin/smoke --command=$(shell stack exec -- which smoke-exe) test

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
