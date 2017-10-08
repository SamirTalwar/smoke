FROM haskell:integer-gmp

COPY bin/smoke /smoke
WORKDIR /var/app
ENTRYPOINT ["/smoke"]
