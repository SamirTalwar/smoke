#!/usr/bin/env ruby

require 'rspec'

TEST_CASE = ARGV.shift
APPLICATION = ARGV.shift

RSpec::Matchers.define :be_one_of do |potential_values|
  potential_value_count = potential_values.length
  match do |actual|
    raise 'No outputs provided.' if potential_value_count == 0
    next potential_values[0] == actual if potential_value_count == 1
    potential_values.include? actual
  end

  failure_message_for_should do |actual|
    next 'no outputs provided' if potential_value_count == 0
    next "expected #{actual.inspect} to be #{potential_values[0].inspect}" if potential_value_count == 1
    "expected #{actual.inspect} to be one of #{potential_values.inspect}"
  end
end

describe TEST_CASE do
  Dir.glob "#{TEST_CASE}/*.in" do |input_file|
    test = input_file[/(?<=#{TEST_CASE}\/).*(?=\.in)/]
    input = IO.read(input_file).strip
    output_files = Dir.glob "#{TEST_CASE}/#{test}.out*"
    potential_outputs = output_files.collect do |output_file|
      IO.read(output_file).strip
    end

    it "handles the #{test} case" do
      IO.popen APPLICATION, 'r+' do |io|
        io.write IO.read(input_file).strip
        io.read.strip.should be_one_of potential_outputs 
      end
    end
  end
end

RSpec::Core::Runner.run ['--color']
