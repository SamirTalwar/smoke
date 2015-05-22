Smoke
=====

We had a problem at work. It was a pretty nice problem to have. We were getting
too many job applicants and we needed to screen them quickly. So we put some
tests online and pointed the <del>guinea pigs</del> <ins>candidates</ins> at
'em.

We quickly found we had another problem: it was taking a lot of developer time
to decide whether we should bring in the furballs for real-life interviews. So
one night, while more than a little tipsy, I wrote *Smoke*: an integration test
framework for console applications.

Why?
----
We let our interview candidates write code in whatever they like: Java, C#,
Python, Ruby&hellip; we've not had a Brainfuck answer yet, but I think I'd just
put them through on sheer awesomeness. I needed a test framework that could
handle any language under the sun. And some we only let out at night (I'm
looking at you, Scala).

I have to stress that this is not a replacement for looking over people's code.
I've put people through that failed every one of my test cases because they
understood the problem and mostly solved it. Similarly, someone that passes
every case but writes Python like people wrote C in the 80s makes me very sad,
despite all the green output from Smoke.

How?
----
At first, I thought about ways to crowbar RSpec into running application tests.
This was a stupid idea. Eventually I decided the only thing every language has
in common is the command-line: every language can pretty easily support
standard input and output (with the obvious exception of Java, which makes
everything difficult).

So in order to run tests against an application, you simply invoke Smoke with
the directory containing the tests, and the command required to invoke the
application.

    smoke tests/calculator 'ruby calculator.rb'

In your test directory, you'll have a bunch of sample inputs along with
expected outputs. So for our hypothetical calculator, you might have a
simple input:

### addition.in
    2 + 2

and the output you expect:

### addition.out
    4

That's it. Add as many tests as you like, and you'll get red or green responses
(along with expected and actual output if it went wrong) for every one.

If you want to push stuff through command-line arguments instead, put them in
a file ending in *.args* instead. For example, renaming *addition.in* to
*addition.args* will mean that *ruby calculator.rb 2 + 2* will be invoked, with
no standard input supplied. You can combine standard input and command-line
arguments.

Enjoy. Any feedback is welcome.
