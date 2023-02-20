[![GitHub Actions Build Status](https://github.com/SamirTalwar/smoke/workflows/Verify/badge.svg)](https://github.com/SamirTalwar/smoke/actions?query=workflow%3AVerify)

# Smoke

_An integration test framework for practically anything._

![Smoke output](./screenshot.png)

Smoke is designed to test anything that can be wrapped with a command-line interface. In practice, this amounts to almost any application or large piece of code. Whatever you're working on, no matter how big or complicated, you can usually wrap a CLI around it with minimum effort.

Smoke works especially well for testing large applications, especially after the fact. It allows you to create regression tests, [golden master tests][testing legacy code with golden master], and other things that make refactoring a legacy application much easier.

It's not a replacement for other, smaller tests. We recommend writing unit tests ([perhaps even first][test-driven development]), especially for new code.

Smoke is distributed under [the MIT license][mit license].

[testing legacy code with golden master]: https://craftedsw.blogspot.co.uk/2012/11/testing-legacy-code-with-golden-master.html
[test-driven development]: https://en.wikipedia.org/wiki/Test-driven_development

## Installation

You can download the latest release from the [Releases][] page.

- The latest Windows release was built on Windows Server 2022.
- The latest macOS release was built on macOS 11 (Big Sur), on x86_64 hardware.
  - If you need native arm64 support, you will need to build it yourself.
- The latest Linux release was built on Ubuntu 20.04, on x86_64 hardware.
  - The binary depends on the dynamic libraries `glibc` and `gmp`.
  - If you're running on a non-glibc-based OS such as Alpine Linux, you will either need to build it yourself or install both `gcompat` and `gmp`.
  - If you need native arm64 support, you will need to build it yourself.

## Building

You can also build it yourself, using either Nix or Stack.

With [Nix][]:

1.  Install Nix.
2.  Run `nix-build -o ./out/build`.
3.  Find Smoke at `./out/build/bin/smoke`.

With [Stack][]:

1.  Install Stack.
2.  Run `stack install --local-bin-path=./out/build`.
3.  Find Smoke at `./out/build/smoke`.

[mit license]: http://samirtalwar.mit-license.org/
[releases]: https://github.com/SamirTalwar/smoke/releases
[nix]: https://nixos.org/nix
[stack]: https://haskellstack.org/

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

### Simple test cases

For a simple example, let's try testing a command-line calculator program.

Our simplest calculator test case looks like this. It's a specification file named _smoke.yaml_ (the file basename is a convention; you can name it anything you want ending in _.yaml_).

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

We use the YAML operator `|` to capture the following indented block as a string. This allows us to easily check for multiline output, and includes the trailing newline, which is useful when dealing with software that typically prints a newline at the end of execution. It also guarantees that we parse the value as a string, and not, for example, as a number, as in the case above.

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

Sometimes the response might be one of a few different values, in which case, we can specify an array of possible outcomes:

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

### Matchers

We don't always want to check the full output; sometimes checking that it contains a given substring is more useful. We can use the `contains:` operator to specify this:

```yaml
tests:
  # ...
  - name: big multiplication
    stdin: |
      961748927 * 982451653
    stdout:
      contains: "1021"
```

Note that we don't use `|` here, as we don't want to capture the trailing newline, which would make this test fail. Instead we use quotes around the value to ensure that the YAML parser treats it as a string, not a number.

Or we can use a regular expression with the `matches:` operator:

```yaml
tests:
  - name: lorem ipsum
    stdout:
      matches: |-
        ^Lorem ipsum .* dolore magna aliqua\.$
```

Here we're using `|-` to similar effect; the `-` means "don't add a trailing newline". Unlike quotes, it doesn't interpret escape sequences (such as `\n`), so we can use `\` without trouble.

You may want to provide options to the regular expression. The currently-supported options are `case-insensitive` (`i`), `comments` (`x`), and `dot-all` (`s`). You can add them as follows:

```yaml
tests:
  - name: lorem ipsum
    stdout:
      matches:
        regex: |-
          Lorem ipsum .* fugiat nulla pariatur\.
        options:
          - case-insensitive
          - dot-all
```

The variant of regular expressions supported is from the ICU library; see [the ICU documentation on Regular Expressions](https://unicode-org.github.io/icu/userguide/strings/regexp.html) for syntax and functionality, including how the options work.

You can also use `equals:` to explicitly specify that we're checking equality, though this is the default.

We can use files to specify the STDIN, STDOUT or STDERR values:

```yaml
tests:
  - name: subtraction
    stdin:
      file: tests/subtraction.in
    stdout:
      file: tests/subtraction.out
```

Using files gives us one big advantage over specifying the content inline: if the tests fail, but the actual output looks correct, we can "bless" the new STDOUT or STDERR with the `--bless` flag. This means you don't have to spend time copying and pasting, and can instead just approve the results automatically.

We can ignore tests (temporarily, we hope) by adding `ignored: true`.

And, of course, you can combine all these techniques together.

### Testing files

Smoke can also test that your application wrote a file.

For example, if we wanted to test that our implementation of the `cp` (copy) command worked, we could write the following:

```yaml
working-directory: .

tests:
  - name: copy a file
    command:
      - cp
    args:
      - input.file
      - output.file
    files:
      - path: output.file
        contents:
          file: input.file
    revert:
      - .
```

You also need to create a file called _input.file_, containing whatever you want.

This test will run `cp input.file output.file`. In doing so, it will set the working directory to the same directory as the _smoke.yaml_ file (so you can run the tests from anywhere, and they'll behave exactly the same). It will then revert the entire contents of the specified directory, `.`, to its state before the test was run.

As with STDOUT and STDERR, you can also use `--bless` to automatically accept the new output if it changes.

Take a look at the [files fixture][] for more examples.

[files fixture]: ./fixtures/files/smoke.yaml

### Running with a shell

Sometimes writing a small program for testing purposes is a bit distracting or over the top. We can specify a string as the contents of a command, which will be passed to the default shell (`sh` on Unix, `cmd` on Windows).

You can override the shell, too, both per test, or at the top-level, by providing a `shell:` section. For example, we could use this to pass the `-e` flag to `bash`, making sure it fails fast:

```yaml
tests:
  - name: use custom shell flags
    command:
      shell:
        - bash
        - -e
      script: |
        echo 'Something.' >&2
        false
        echo 'Something else.' >&2
    exit-status: 1
    stderr: |
      Something.
```

You could even set your shell to `python` or `ruby` (or your favorite scripting language) to allow you to embed scripts of any kind in your Smoke specification.

You can find more examples in the [local shell fixture][] and [global shell fixture][].

[local shell fixture]: ./fixtures/shell/local.yaml
[global shell fixture]: ./fixtures/shell/global.yaml

### Filtering output

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
        - jq
        - .args
```

Now my test passes every time.

You can also specify the filter as an inline script by using a string rather than an array. It will be run with `sh -c`. We prefer the array structure, as it's more portable.

There are more examples in the [processing-filters fixture][].

[httpbin.org]: https://httpbin.org/
[processing-filters fixture]: ./fixtures/processing-filters/smoke.yaml

### Further examples

If you're looking for more examples, take a look in the `fixtures` directory.

## Running Tests

In order to run tests against an application, you simply invoke Smoke with the directory containing the tests. Given the tests in the _test_ directory, we would run the tests as follows:

```sh
smoke test
```

You can provide individual file names instead:

```sh
smoke test/one.yaml test/two.yaml
```

If you want to run all tests matching a pattern, you can use `find` to find the files, and then pass them to Smoke (type `man find` for help):

```sh
smoke $(find test -name '*.smoke.yaml')
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

You will need to set up Nix as above, and enter a Nix shell with `nix-shell`, or use `lorri` with `direnv`.

We dog-food. Smoke is tested using itself.

Before committing, these commands should be run, and any failures should be fixed:

```sh
cabal v2-update # Update the Cabal packages.
make reformat   # Reformats the code using ormolu and nixpkgs-fmt.
make build      # Builds the application using Cabal.
make test       # Run the unit tests.
make spec       # Tests the application using itself, with the tests in the "spec" directory.
make lint       # Lints the code using HLint.
```

(You can typically just run `make reformat check` to trigger them all.)

Some development tools, such as `ormolu`, don't work on Windows, so we encourage you to develop on a Nix-compatible environment. However, if you need to write or test some code on Windows, you can always read the _Makefile_ to figure out what commands to run.

Smoke should work on Linux and macOS without any issue. Almost all features should also work on Windows, with the exception of allowing scripts as commands. This is due to a (quite reasonable) limitation of Windows; you can't make text files executable.

[contributor covenant]: http://contributor-covenant.org/version/1/4/
