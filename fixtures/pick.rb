#!/usr/bin/env ruby

if ARGV.empty?
  puts $stdin.read.lines.sample
else
  puts ARGV.sample
end
