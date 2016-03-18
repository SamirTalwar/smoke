#!/usr/bin/env ruby

tokens = $stdin.read.split
puts tokens[0].to_i.send(tokens[1].to_sym, tokens[2].to_i)
