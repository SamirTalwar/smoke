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

def broken(n)
  if n % 111 == 0
    n / 2
  else
    n
  end
end

if ARGV[0] == '--broken'
  start = ARGV[1].to_i
  counter = start.downto(1).map { |n| broken(n) }
else
  start = ARGV[0].to_i
  counter = start.downto(1)
end

counter.each do |n|
  puts "#{bottles(n).capitalize} of beer on the wall, #{bottles n} of beer."
  puts "Take one down and pass it around, #{bottles(n - 1)} of beer on the wall."
end

puts 'No more bottles of beer on the wall, no more bottles of beer.'
puts "Go to the store and buy some more, #{bottles counter.first} of beer on the wall."
