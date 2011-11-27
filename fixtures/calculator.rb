#!/usr/bin/env ruby

puts(ARGV[0].to_i.send ARGV[1].to_sym, ARGV[2].to_i)
