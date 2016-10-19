[![Build Status](https://travis-ci.org/SamirTalwar/Smoke.svg?branch=master)](https://travis-ci.org/SamirTalwar/Smoke)

# Smoke

*An integration test framework for console applications.*

![Smoke output](https://s3-eu-west-1.amazonaws.com/samirtalwar-smoke/screenshot.png)

## Installation

Currently, Smoke is in alpha, and as such is not packaged. You can use it with your own projects by downloading the [*smoke* binary][bin/smoke], [Smoke Windows launcher][bin/smoke.bat] and the [LICENSE][] file and committing them with your code, or adding this repository as a Git submodule and committing the reference.

The *smoke* binary requires Ruby 1.9.3 or greater. It does not require any additional gems.

You can also run Smoke using [Docker][] without any further installation. See below for more information.

Smoke is distributed under [the MIT license][the MIT license].

[bin/smoke]: https://raw.githubusercontent.com/SamirTalwar/Smoke/master/bin/smoke
[bin/smoke.bat]: https://raw.githubusercontent.com/SamirTalwar/Smoke/master/bin/smoke.bat
[LICENSE]: https://raw.githubusercontent.com/SamirTalwar/Smoke/master/LICENSE
[Docker]: https://www.docker.com/
[the MIT license]: http://samirtalwar.mit-license.org/

## Writing Test Cases

A test case consists of *input* and *expected output*. It is constructed of a number of files with the same name and different extensions.

First off, you need to specify the *command* itself.

  * The command is specified in a file named `command`, with parameters each on a new line. It is executed from the current working directory.
  * The command can be overriden for each individual test case by creating a file with the `.command` extension.

Input can come in two forms: *standard input* and *command-line arguments*.

  * Standard input is specified by naming the file with the extension `.in`.
  * Command-line arguments are specified in a file with the `.args` extension, one per line.

Outputs that can be observed by Smoke consist of *standard output*, *standard error* and the *exit status* of the program.

  * Expected standard output is specified with the `.out` extension. Alternatively, multiple possible expected outputs can be specified by using an extension that starts with `.out`—for example, `.out-one`, `.out2` and `.outc`. If there are multiple outputs, a match with any of them will be considered a success.
  * Expected standard error uses the `.err` extension, but otherwise works in exactly the same way as expected standard output.
  * The expected exit status is a file with the `.status` extension. It contains a single number between `0` and `255`.

At least one of standard output and standard error must be specified, though it can be empty. If no exit status is specified, it will be assumed to be `0`.

### Example: Calculator

Our simplest calculator test case consists of three files:

#### test/command:

    ruby
    calculator.rb

#### test/addition.in:

    2 + 2

#### test/addition.out:

    4

That's it.

We might want to assert that certain things fail. For example, postfix notation should fail because the second token is expected to be an operator. In this example, our calculator is expected to produce a semi-reasonable error message and exit with a status of `2` to signify a parsing error.

#### test/postfix-notation-fails.in:

    5 3 *

#### test/postfix-notation-fails.err:

    "3" is not a valid operator.

#### test/postfix-notation-fails.status:

    2

## Running Tests

In order to run tests against an application, you simply invoke Smoke with the command required to invoke the application, and the directory containing the tests. Given an application that is invoked with `ruby bin/calculator.rb`, and the tests in the *test* directory, we would run the tests as follows:

    smoke test

Tests can also be passed on an individual basis:

    smoke test/addition test/postfix-notation-fails

To override the command, or to specify it on the command line in place of the `command` file, you can use the `--command` switch:

    smoke --command='ruby calculator.rb' test

Smoke will exit with a code of `0` if all tests succeed, `1` if any test fails, or `2` if the invocation of Smoke itself was not understood (for example, if only one argument is provided).

Output will be in color if outputting to a terminal. You can force color output on or off with the `--color` and `--no-color` switches.

In order to run Smoke with Docker instead, you would change the command as follows:

    docker run --rm -it -v $PWD:/var/app samirtalwar/smoke test

Enjoy. Any feedback is welcome.

## Origins

We had a problem at work. It was a pretty nice problem to have. We were getting too many job applicants and we needed to screen them quickly. So we put some tests online and pointed the <del>guinea pigs</del> <ins>candidates</ins> at 'em.

We quickly found we had another problem: it was taking a lot of developer time to decide whether we should bring in the furballs for real-life interviews. So one night, while more than a little tipsy, I wrote *Smoke*.

We let our interview candidates write code in whatever they like: Java, C#, Python, Ruby&hellip; I needed a test framework that could handle any language under the sun. At first, I thought about ways to crowbar RSpec into running application tests. This was a stupid idea. Eventually I decided the only thing every language has in common is the command-line: every language can pretty easily support standard input and output (with the obvious exception of Java, which makes everything difficult).

I have to stress that this is not a replacement for looking over people's code. I've put people through that failed every one of my test cases because they understood the problem and mostly solved it. Similarly, someone that passes every case but writes Python like people wrote C in the 80s makes me very sad, despite all the green output from Smoke.

## Contributing

Issues and pull requests are very welcome. Please don't hesitate.

Developers of Smoke pledge to follow the [Contributor Covenant][].

We dog-food. You can run all of Smoke's smoke tests using:

    rake test

On Windows, run this instead:

    bin\smoke --command=bin\smoke test

Smoke development follows a few rules:

  * The *smoke* binary is a single file without any dependencies. No gems are used.
  * Smoke should work on Linux and Mac OS without any issue. Most features should also work on Windows, and we hope to get it to feature parity soon.

[Contributor Covenant]: http://contributor-covenant.org/version/1/4/
