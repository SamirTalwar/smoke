[![Travis Build Status](https://travis-ci.org/SamirTalwar/smoke.svg?branch=master)](https://travis-ci.org/SamirTalwar/smoke) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/9m04ffr3h65cviht/branch/master?svg=true)](https://ci.appveyor.com/project/SamirTalwar/smoke)

# Smoke

_An integration test framework for practically anything._

![Smoke output](./screenshot.png)

Smoke is designed to test anything that can be wrapped with a command-line interface. In practice, this amounts to almost any application or large piece of code. Whatever you're working on, no matter how big or complicated, you can usually wrap a CLI around it with minimum effort.

Smoke works especially well for testing large applications, especially after the fact. It allows you to create regression tests, [golden master tests][testing legacy code with golden master], and other things that make refactoring a legacy application much easier.

It's not a replacement for other, smaller tests. We recommend writing unit tests ([perhaps even first][test-driven development]), especially for new code.

[testing legacy code with golden master]: https://craftedsw.blogspot.co.uk/2012/11/testing-legacy-code-with-golden-master.html
[test-driven development]: https://en.wikipedia.org/wiki/Test-driven_development

## Installation

You can download the latest release from the [Releases][] page.

You can also build it yourself.

1.  Install [Nix][].
2.  Run `nix-build`.

Smoke is distributed under [the MIT license][mit license].

[releases]: https://github.com/SamirTalwar/smoke/releases
[nix]: https://nixos.org/nix
[mit license]: http://samirtalwar.mit-license.org/

## Writing Test Cases

A test case consists of _input_ and _expected output_. It's made with a YAML file.

First off, you need to specify the _command_ itself. The command is the program to be run (and any common arguments). It is executed from the current working directory. The command can be overriden for each individual test case too.

Input can come in two forms: _standard input_ and _command-line arguments_.

- Command-line arguments are appended to the command to run it.
- Standard input is piped into the program on execution.

Outputs that can be observed by Smoke consist of _standard output_, _standard error_ and the _exit status_ of the program. These are captured by running the program, then compared to the expected values specified. Any difference results in a test failure.

- Expected standard output is compared with the actual standard output. Alternatively, multiple possible expected outputs can be specified. If there are multiple outputs, a match with any of them will be considered a success.
- Expected standard error works in exactly the same way as expected standard output.
- The expected exit status is a single number between `0` and `255`.

At least one of standard output and standard error must be specified, though it can be empty. If no exit status is specified, it will be assumed to be `0`.

### Example: Calculator

Our simplest calculator test case looks like this. It's a file named _smoke.yaml_ (the file basename is a convention; you can name it anything you want ending in _.yaml_).

```yaml
command:
  - ruby
  - calculator.rb

tests:
  - name: addition
    stdin: |
      2 + 2
    stdout: |
      4
```

That's it.

We might want to assert that certain things fail. For example, postfix notation should fail because the second token is expected to be an operator. In this example, our calculator is expected to produce a semi-reasonable error message and exit with a status of `2` to signify a parsing error.

```yaml
tests:
  # ...
  - name: postfix-notation-fails
    stdin: |
      5 3 *
    exit-status: 2
    stderr: |
      "3" is not a valid operator.
```

Sometimes the response might be one of a few different values, in which case, I can specify an array of possible outcomes:

```yaml
tests:
  # ...
  - name: square root
    stdin: |
      sqrt(4)
    stdout:
      - |
        2
      - |
        -2
```

You can use files to specify the STDIN, STDOUT or STDERR values:

```yaml
tests:
  - name: subtraction
    stdin:
      file: tests/subtraction.in
    stdout:
      file: tests/subtraction.out
```

And, of course, you can combine all these techniques together.

### Example: HTTP requests

Sometimes, things aren't quite so deterministic. When some of the output (or input) is meaningless, or if there's just too much, you can specify filters to transform the data.

[httpbin.org][] provides a simple set of HTTP endpoints that repeat what you tell them to. When I `GET https://httpbin.org/get?foo=bar`, the response body looks like this:

```json
{
  "args": {
    "foo": "bar"
  },
  "headers": {
    "Accept": "*/*",
    "Accept-Encoding": "gzip, deflate",
    "Connection": "close",
    "Host": "httpbin.org",
    "User-Agent": "HTTPie/1.0.0"
  },
  "origin": "1.2.3.4",
  "url": "https://httpbin.org/get?foo=bar"
}
```

Unfortunately, because of the `"origin"`, this isn't very testable, as that might as well be random data. Given that I only really care about the `"args"` property, I can use `jq` to just extract that part:

```yaml
command:
  - http

tests:
  - name: get
    args:
      - GET
      - https://httpbin.org/get?foo=bar
    stdout:
      contents: |
        {
          "foo": "bar"
        }
      filter:
        - "jq"
        - ".args"
```

Now my test passes every time.

You can also specify the filter as an inline script by using a string rather than an array. It will be run with `sh -c`. We prefer the array structure, as it's more portable.

[httpbin.org]: https://httpbin.org/

### Further examples

If you're looking for more examples, take a look in the `fixtures` directory.

## Running Tests

In order to run tests against an application, you simply invoke Smoke with the directory containing the tests. Given the tests in the _test_ directory, we would run the tests as follows:

```sh
smoke test
```

Tests can also be passed on an individual basis:

```sh
smoke test/smoke.yaml@addition test/smoke.yaml@subtraction
```

To override the command, or to specify it on the command line instead of the `command` property, you can use the `--command` option:

```
smoke --command='ruby calculator.rb' test
```

Bear in mind that Smoke simply splits the argument to the `--command` option by whitespace, so quoting, escaping, etc. will not work. For anything complicated, use a file instead.

Smoke will exit with a code of `0` if all tests succeed, or non-zero if any test fails, or if the invocation of Smoke itself was not understood (for example, if no test locations are provided).

Output will be in color if outputting to a terminal. You can force color output on or off with the `--color` and `--no-color` switches.

Enjoy. Any feedback is welcome.

## Origins

We had a problem at work. It was a pretty nice problem to have. We were getting too many job applicants and we needed to screen them quickly. So we put some tests online and pointed the <del>guinea pigs</del> <ins>candidates</ins> at 'em.

We quickly found we had another problem: it was taking a lot of developer time to decide whether we should bring in the furballs for real-life interviews. So one night, while more than a little tipsy, I wrote _Smoke_.

We let our interview candidates write code in whatever they like: Java, C#, Python, Ruby… I needed a test framework that could handle any language under the sun. At first, I thought about ways to crowbar RSpec into running tests for applications in any and all languages. This was a stupid idea. Eventually I decided the only thing every language has in common is the command line: every language can pretty easily support standard input and output (with the obvious exception of Java, which makes everything difficult).

I have to stress that this is not a replacement for looking over people's code. I've invited people for further interview even when failed every one of my test cases, because they understood the problem and mostly solved it. Similarly, someone that passes every case but writes Python like people wrote C in the 80s makes me very sad, despite all the green output from Smoke.

## Contributing

Issues and pull requests are very welcome. Please don't hesitate.

Developers of Smoke pledge to follow the [Contributor Covenant][].

You will need to set up Nix as above, and enter a Nix shell with `nix-shell`.

We dog-food. Smoke is tested using itself.

Before committing, these commands should be run, and any failures should be fixed:

```sh
cabal v2-update # Update the Cabal packages.
make reformat   # Reformats the code using hindent.
make build      # Builds the application using Cabal.
make test       # Run the unit tests.
make spec       # Tests the application using itself, with the tests in the "spec" directory.
make lint       # Lints the code using HLint.
```

(You can typically just run `make reformat check` to trigger them all.)

Some commands, such as `cabal2nix` and `ormolu`, don't work on Windows, so we encourage you to develop on a Nix-compatible environment. However, you can always read the _Makefile_ to figure out what commands to run.

Smoke should work on Linux and macOS without any issue. Almost all features should also work on Windows, with the exception of allowing scripts as commands. This is due to a (quite reasonable) limitation of Windows; you can't make text files executable.

[contributor covenant]: http://contributor-covenant.org/version/1/4/
