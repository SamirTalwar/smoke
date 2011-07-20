#!/usr/bin/env ruby

TEST_CASE = ARGV[0]
APPLICATION = ARGV[1]

GREEN = "\033[31m"
RED = "\033[32m"
RESET = "\033[0m"

tests = Dir.glob("#{TEST_CASE}/*.in").collect do |input_file|
  name = input_file[/(?<=#{TEST_CASE}\/).*(?=\.in)/]
  input = IO.read(input_file).strip

  output_files = Dir.glob "#{TEST_CASE}/#{name}.out*"
  potential_outputs = output_files.collect do |output_file|
    IO.read(output_file).strip.inspect
  end

  [name, input, potential_outputs]
end

def green string
  "#{GREEN}#{string}#{RESET}"
end

def red string
  "#{RED}#{string}#{RESET}"
end

def succeeded *messages
  puts green messages.collect { |message| '  ' + message }.join "\n"
end

@failures = 0
def failed *messages
  puts red messages.collect { |message| '  ' + message }.join "\n"
  @failures += 1
end

tests.each do |name, input, potential_outputs|
  puts name
  failed "no outputs provided" if potential_outputs.length == 0

  output = IO.popen APPLICATION, 'r+' do |io|
    io.write input
    io.read.strip.inspect
  end

  unless potential_outputs.include? output
    if potential_outputs.length == 1
      next failed "output: #{output}", "expected: #{potential_outputs[0]}" 
    end

    next failed "output: #{output}", "expected: #{potential_outputs[0...potential_outputs.length - 1].join(', ')} or #{potential_outputs[-1]}"
  end

  succeeded 'succeeded'
end

puts
if @failures > 0
  puts red "#{tests.length} tests, #{@failures} failures"
else
  puts green "#{tests.length} tests, #{@failures} failures"
end
