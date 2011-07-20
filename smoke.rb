#!/usr/bin/env ruby

TEST_CASE = ARGV[0]
APPLICATION = ARGV[1]

tests = Dir.glob("#{TEST_CASE}/*.in").collect do |input_file|
  name = input_file[/(?<=#{TEST_CASE}\/).*(?=\.in)/]
  input = IO.read(input_file).strip

  output_files = Dir.glob "#{TEST_CASE}/#{name}.out*"
  potential_outputs = output_files.collect do |output_file|
    IO.read(output_file).strip
  end

  [name, input, potential_outputs]
end

def succeeded message
  puts "\033[32m#{message}\033[0m"
end

@failures = 0
def failed message
  puts "\033[31m#{message}\033[0m"
  @failures += 1
end

tests.each do |name, input, potential_outputs|
  failed "#{name}: no outputs provided" if potential_outputs.length == 0

  output = IO.popen APPLICATION, 'r+' do |io|
    io.write input
    io.read.strip
  end

  unless potential_outputs.include? output
    next failed "#{name}:\n  output: #{output.inspect}\n  expected: #{potential_outputs[0].inspect}" if potential_outputs.length == 1
    next failed "#{name}:\n  output: #{output.inspect}\n  expected: #{potential_outputs[0...potential_outputs.length - 1].join(', ')} or #{potential_outputs[-1]}"
  end

  succeeded name
end

puts
if @failures > 0
  failed "#{tests.length} tests, #{@failures} failures"
else
  succeeded "#{tests.length} tests, #{@failures} failures"
end
