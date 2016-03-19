#!/usr/bin/env ruby

values = ARGV.empty? ? $stdin.read.lines.collect(&:strip) : ARGV
puts values[rand(values.length)]
