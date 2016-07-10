#!/usr/bin/env ruby

def bottles(n)
  case n
  when 0
    'no more bottles'
  when 1
    '1 bottle'
  else
    "#{n} bottles"
  end
end

start = ARGV[0].to_i
start.downto(1).each do |n|
  puts "#{bottles(n).capitalize} of beer on the wall, #{bottles n} of beer."
  puts "Take one down and pass it around, #{bottles(n - 1)} of beer on the wall."
end

puts 'No more bottles of beer on the wall, no more bottles of beer.'
puts "Go to the store and buy some more, #{bottles start} of beer on the wall."
