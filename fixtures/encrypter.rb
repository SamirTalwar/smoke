#!/usr/bin/env ruby

def rotate char
  base = 'a'.ord
  ((char.ord - base + 13) % 26 + base).chr
end

def rot13 string
  in_escape = false
  string.chars.collect { |char|
    if char == "\e"
      in_escape = true
      next char
    end

    if in_escape
      in_escape = false if char == "m"
      next char
    end

    next rotate(char.downcase).upcase if ('A'..'Z').include? char
    next rotate(char) if ('a'..'z').include? char
    next char
  }.join
end

puts rot13 $stdin.read
