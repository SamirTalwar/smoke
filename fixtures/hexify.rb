#!/usr/bin/env ruby

puts gets.chars.collect { |char| char.ord.to_s(16) }.join.upcase
