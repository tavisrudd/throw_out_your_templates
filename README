This README is copied from the module docstring, you should read that
and the code instead.  This is just a preview for bitbucket ...
--------------------------------------------------------------------------------
Python has many template languages and most Python developers have
used several or written their own (I'm guilty, Cheetah).  Very few
Python web developers use Python itself for HTML/view generation.
It's argued that template languages, compared to Python:

  a) provide better separation between domain model / application
     logic code and the presentation layer

  b) are easier to read and less prone to spaghetti code, as Python's
     built-in string interpolation/concatenation tools aren't well
     suited for HTML generation.

  c) make it easier to handle HTML escaping and character encoding
     correctly

  d) solve work-flow issues when collaborating with non-programmers
     (designers, writers, translators, etc.) who understand neither
     Python nor the tools required to work with it (editor, etc.).

  e) can provide a sandbox that insulates you from the mistakes or
     malicious code injections of unskilled or untrusted contributors
     (non-programmers, junior developers, contractors, etc.).

I used to believe these arguments but recently I've come to see them
as mostly dogma. Consider the example code in this module as evidence
for my counterargument:

It is easy to generate correct HTML from plain Python without a)
blurring the separation between the domain/application and
presentation layers, b) creating spaghetti code (HTML fragments
embedded in strings, a mess of str concats, etc.), or c) screwing up
the HTML-escaping.  Furthermore, there are big advantages to doing so:

  1) Full tool-chain support:
     - editor support: syntax highlighting, code nav tools,
       auto-completion, intellisense/eldoc, snippets, refactoring &
       search/replace tools, etc.
     - static code analyzers: pyflakes, pylint (especially with
       flymake in Emacs, or the equivalent in other editors, which
       highlights syntax errors and undefined variables as you type)
     - debuggers
     - testing/coverage tools: pycoverage, etc.

     Tool-chain support for template languages is patchy at best.  Even
     if it were perfect, it's yet another thing to learn, configure
     and maintain.

     Finally, the Python interpreter itself understands the code,
     unlike template src that is just an opaque string to it.  In fact
     with the example shown in this module, Python 'understands' far
     more about what you are doing than any template language can.
     The HTML in template src code is just an opaque string to a
     template parser, unless the parser is an XML parser and your
     template syntax is valid XML.

  2) Python is extremely expressive and requires far fewer keystrokes
     to output HTML than a template lang.  This is especially true of
     Django templates and its restrictive syntax and system of custom
     template tags.

  3) Good Python tools for view generation can support higher levels
     of abstraction and more declarative, 'intentional' coding styles
     than possible in templates, which are usually quite imperative.
     This can result in a more flexible, more readable, more testable
     and more reusable presentation layer.  This module attempts to
     achieve that by encouraging a strong separation between the
     declaration of a view and the definition of how it will be
     serialized.  See the final example.

  4) The implementation of a pure-Python view generator is far easier
     to grok, debug, and maintain than a template language
     implementation.  There's no lexer, parser, compiler /
     code-generator, or interpreter involved.  The core of this module
     is less than 250 sloc according to sloccount, while Django
     templates has roughly 2700, Cheetah's core is about 4000 sloc,
     Mako is also 4000, and Jinja2 seems to be almost 6000 (excluding
     the test suite).

My rebuttal to argument (d) (work-flow) is YAGNI!  The vast majority
of people writing templates are developers.  Designers usually
contribute mockups or css.  The rare designers who do actually work
with templates are fully capable of learning a subset of Python,
especially when it's less complicated than the template language
syntax.

Anyone who buys argument (e) (sandboxing and gentle error handling) is
living in the past by assuming that server-side domain code is somehow
more important than the presentation layer or what happens on the
browser and that it should be held to a higher standard.  A bug is a
bug.  These days, mistyped template variable names, incorrect
HTML/Javascript, missing or incorrect parts of the UI are just as
unacceptable as bugs or syntax errors in the back-end.  They should be
detected and handled early. Presentation layer code is just as
important as the back-end and should be held to the same standards.
Furthermore, if someone can include Javascript in the template there
is no sandboxing.  The same people I've heard using this argument are
also advocates of code-review, so I'm confused by their logic.

... Read the module docstring and code for the rest ...

