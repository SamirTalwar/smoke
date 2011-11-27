#!/usr/bin/env ruby

$stdin.read.split("\n").each do |line|
  puts line.chars.collect { |char| char.ord.to_s(16) }.join.upcase
end
