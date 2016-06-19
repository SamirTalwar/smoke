FROM ruby:slim

COPY bin/smoke /smoke
WORKDIR /var/app
ENTRYPOINT ["/smoke"]
