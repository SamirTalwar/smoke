FROM nixos/nix

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

WORKDIR /src
COPY *.nix ghc.version ./
RUN nix-shell --pure --run true

RUN nix-shell --pure --run 'cabal v2-update'

COPY smoke.cabal cabal.project.freeze ./
RUN nix-shell --pure --run 'cabal v2-build --only-dependencies'

ENV LANG=en_US.utf8
COPY . ./
ENTRYPOINT nix-shell --pure --keep LANG --run "$0 $*"
