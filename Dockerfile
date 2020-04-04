FROM nixos/nix

WORKDIR /src
COPY .gitignore *.nix ghc.version smoke.cabal ./
COPY nix nix/
RUN nix-build --no-out-link shell.nix

ENV LANG=C.UTF-8
COPY LICENSE README.md ./
COPY src src/
RUN nix-build --out-link /opt/smoke .
ENTRYPOINT ["/opt/smoke/bin/smoke"]
