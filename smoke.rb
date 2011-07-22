#!/usr/bin/env ruby

GREEN = "\033[32m"
RED = "\033[31m"
RESET = "\033[0m"

def read_tests test_case
  Dir.glob("#{test_case}/*.in").collect do |input_file|
    name = input_file[(test_case.length + 1)...(input_file.length - 3)]
    input = IO.read(input_file).strip

    output_files = Dir.glob "#{test_case}/#{name}.out*"
    potential_outputs = output_files.collect do |output_file|
      IO.read(output_file).strip.inspect
    end

    [name, input, potential_outputs]
  end
end

def run_tests tests, application
  tests.each do |name, input, potential_outputs|
    puts name
    next failed "no outputs provided" if potential_outputs.length == 0

    output = IO.popen application, 'r+' do |io|
      io.write input
      io.close_write
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
end

def print_summary
  puts
  if @failures > 0
    puts red "#{@successes + @failures} tests, #{@failures} failures"
  else
    puts green "#{@successes + @failures} tests, #{@failures} failures"
  end
end

def green string
  "#{GREEN}#{string}#{RESET}"
end

def red string
  "#{RED}#{string}#{RESET}"
end

@successes = 0
def succeeded *messages
  puts green messages.collect { |message| '  ' + message }.join "\n"
  @successes += 1
end

@failures = 0
def failed *messages
  puts red messages.collect { |message| '  ' + message }.join "\n"
  @failures += 1
end


run_tests(read_tests(ARGV[0]), ARGV[1])
print_summary
