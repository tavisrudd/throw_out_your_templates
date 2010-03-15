# -*- coding: utf-8 -*-
# pylint: disable-msg=R0922

# Copyright (c) 2007-present, Damn Simple Solutions Ltd.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#     * Neither the name of Damn Simple Solutions Ltd. nor the names
#       of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written
#       permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
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

The basic usage work-flow for this module is:

1) Use Python to build a tree of objects (HTML elements, standard python
   types, your own types, whatever ...) that needs to be serialized into
   HTML.
   e.g.: tree = [doctype,
                 html[head[meta(charset='UTF-8')],
                      body[div[u'content', more_content_from_a_py_var]]]]

2) Build a `VisitorMap` that declares how each `type` should be
   serialized, or use an existing one.

3) Serialize the tree via a serialize function like this:
  def serialize(tree, visitor_map=default_visitors_map,
                input_encoding='utf-8'):
      return Serializer(visitor_map, input_encoding).serialize(tree)
      # returns unicode

  or in a WSGI context:

  def serialize(tree, wsgi_env):
      return Serializer(
          visitor_map=get_visitor_map(wsgi_env),
          input_encoding=get_input_encoding(wsgi_env)
          ).serialize(tree).encode(get_output_encoding(wsgi_env))

  The `tree` argument here doesn't have to be an HTML element.  It can be
  any Python type for which the visitor_map has a visitor.

This module is written in a semi-literate style and its code is
intended to be read:

Table Of Contents
  Part I: The Core
  1: str/unicode wrappers used to prevent double-escaping.
  2: Serializer class (a tree walker that uses the visitor pattern to
     serialize what it walks into properly escaped unicode)
  3: VisitorMap (a dict subclass that maps Python types to visitors)
  4: Default serialization visitors for standard python types

  Part II: Frosting
  5: Declarative classes for creating a DOM-like tree of XML/HTML
  6: Visitors for the XML/HTML elements, etc.

  Part III: Examples
  7: Helpers for examples
  8: Basic examples
  9: Extended example using some fictional model data

The tag syntax in sections 5 to 9 comes from Donovan Preston's 'Stan'
(part of Twisted / Nevow) and Cliff Wells' 'Brevé'.  If you don't like
it, please just remember my main argument and use your imagination to
dream up something better.  Lisp / scheme programmers are using
similar embedded DSLs for HTML-generation.  S-expressions and the
code-as-data / data-as-code philosophy make such a style very natural
in Lisps.  My argument and this code are an echo of what Stan, Brevé
and various lisp libraries have been doing for a long time.  (see
http://www.kieranholland.com/code/documentation/nevow-stan/
http://breve.twisty-industries.com/ and
http://article.gmane.org/gmane.lisp.scheme.plt/16412)

I find this `visitor pattern` variation much more interesting than this
particular tree building syntax.  Kudos to Python's dynamic, yet
strong and introspectable type system for enabling it.  It can be used
with other tree building styles or even to serialize to output formats
other than XML/HTML.  It is especially interesting when combined with
concepts from context-oriented programming and lean-programming.  If
you defer as many rendering choices as possible to the visitors and
visitor_map (lean programming's 'decide as late as possible') you gain
the ability to radically alter the view based on the current context
(think WSGI request environment), without having to change your view
declarations. Because visitors are registered per Python type rather
than per page / template, you have very fine-grained control. For
example, you can register visitors that are appropriate for the
current user's locale (for numbers, dates, times, images,
lazy-gettext-strings, etc), visitors that are appropriate for the
current user's permission levels or preferences, and visitors that
present extra information in design-mode or debug-mode.

If the approach in this module interests you, you may be interested in
my Cython/Pyrex optimized version. This pure Python version is roughly
twice as fast as Django templates.  The Cython version is 20 to 30
times faster than Django depending on usage: on par with Cheetah and
Mako.  The Cython version comes with full unit tests and a little
benchmark suite.  I'll be releasing it under the BSD license on
bitbucket and PyPI sometime before April 2010.  I have been using it
in production for the past 3 years.  However, I haven't chosen a name
for it yet.  If you have any suggestions, please share them with me.

There are a few uses cases where I still use and appreciate templates:
  - when composing a set of pages that contain lots of text
    sprinkled with the occasional variable and light markup

  - when composing email templates

Another option for these use cases is to put any large text blocks in
module level constants (to avoid having strange indentation) and use
Markdown, Textile, ReST, or something similar to handle the markup
prior to labeling them as `safe_unicode` (see below) that doesn't need
any further escaping.

p.s. I discovered after writing this that Cliff Wells wrote a similar
rant back in 2007:  http://www.enemyofthestatement.com/post/by_tag/29

p.p.s Christopher Lenz wrote a good reply to Cliff's post:

    In the end, the reason why I personally wouldn't use something like
    Stan or Breve is because I actually want to work directly with the
    HTML, CSS, and Javascript in my application. I want my text editor
    of choice  to be able to assist me with all the tools it provides
    for working with markup, including support for embedded CSS and
    Javascript. I want to be able to quickly preview a template by
    opening it directly in the browser, without having to run it
    through the template engine first. When I'm working on a template,
    I want to be using HTML, not  Python.
    -- http://www.cmlenz.net/archives/2007/01/genshi-smells-like-php

"""
from __future__ import with_statement
import types
from types import InstanceType
from decimal import Decimal
from cgi import escape as xml_escape

__author__ = 'Tavis Rudd <tavis@damnsimple.com>'

def get_default_encoding():
    return 'utf-8'

################################################################################
# 1: str/unicode wrappers used to prevent double-escaping. This is the
# same concept as django.utils.safestring and webhelpers.html.literal
class safe_bytes(str):
    def decode(self, *args, **kws):
        return safe_unicode(super(safe_bytes, self).encode(*args, **kws))

    def __add__(self, o):
        res = super(safe_bytes, self).__add__(o)
        if isinstance(o, safe_unicode):
            return safe_unicode(res)
        elif isinstance(o, safe_bytes):
            return safe_bytes(res)
        else:
            return res

class safe_unicode(unicode):
    def encode(self, *args, **kws):
        return safe_bytes(super(safe_unicode, self).encode(*args, **kws))

    def __add__(self, o):
        res = super(safe_unicode, self).__add__(o)
        return (safe_unicode(res)
                if isinstance(o, (safe_unicode, safe_bytes)) else res)

################################################################################
# 2: Serializer
class Serializer(object):
    """A tree walker that uses the visitor pattern to serialize what
    it walks into properly escaped unicode.
    """
    def __init__(self, visitor_map=None, input_encoding=None):
        if visitor_map is None:
            visitor_map = default_visitors_map.copy()
        self.visitor_map = visitor_map
        self.input_encoding = (input_encoding or get_default_encoding())
        self._safe_unicode_buffer = []

    def serialize(self, obj):
        """Serialize an object, and its children, into sanitized unicode."""
        self._safe_unicode_buffer = []
        self.walk(obj)
        return safe_unicode(u''.join(self._safe_unicode_buffer))

    def walk(self, obj):
        """This method is called by visitors for anything they
        encounter which they don't explicitly handle.
        """
        visitor = self.visitor_map.get_visitor(obj)
        if visitor:
            visitor(obj, self) # ignore return value
        else:
            raise TypeError('No visitor found for %s'%repr(obj))

    def emit(self, escaped_unicode_output):
        """This is called by visitors when they have escaped unicode
        to output.
        """
        self._safe_unicode_buffer.append(escaped_unicode_output)

    def emit_many(self, output_seq):
        self._safe_unicode_buffer.extend(output_seq)

################################################################################
# 3: VisitorMap
class VisitorMap(dict):
    """Maps Python types to visitors that know how to serialize them.

    A `VisitorMap` can be chained to a `parent_map` that it will
    fall-back to if it doesn't have a visitor registered for a
    specific type (or one of that types base classes).
    """
    def __init__(self, map_or_seq=(), parent_map=None):
        super(VisitorMap, self).__init__(map_or_seq)
        self.parent_map = parent_map

    def get_visitor(self, obj, use_default=True):
        """Return the visitor callable registered for `type(obj)`.

        If no exact match is found, it will look for a visitor
        registered on a base-type in `type(obj).__mro__`.  If that
        fails and the VisitorMap has a `parent_map`,
        `parent_map.get_visitor(obj, use_default=False)` will be
        called.

        If all of the above fails, it returns the `DEFAULT` visitor or
        `None`.
        """
        py_type = type(obj)
        result = (self.get(py_type)
                  or self._get_parent_type_visitor(obj, py_type))
        if result:
            return result
        elif self.parent_map is not None:
            result = self.parent_map.get_visitor(obj, False)
        if not result and use_default:
            result = self.get(DEFAULT)
            if not result and self.parent_map is not None:
                result = self.parent_map.get(DEFAULT)
        return result

    def _get_parent_type_visitor(self, obj, py_type):
        if py_type is InstanceType: # support old-style classes
            m = [t for t in self if isinstance(obj, t)]
            for i, t in enumerate(m):
                if not any(t2 for t2 in m[i+i:]
                           if t2 is not t and issubclass(t2, t)):
                    return self[t]
        else: # newstyle type/class
            for base in py_type.__mro__:
                if base in self:
                    return self[base]

    def copy(self):
        return self.__class__(super(VisitorMap, self).copy())

    def as_context(self, walker, set_parent_map=True):
        """Returns as context manager for use with 'with' statements
        inside visitor functions.

        It allows you to define a set of visitor mappings that only
        apply within the current visitor's context and have all other
        mappings looked up in the exising visitor_map.  See
        `visit_xml_cdata` an example.
        """
        return _VisitorMapContextManager(self, walker, set_parent_map)

    def register(self, py_type, visitor=None):
        """If both args are passed, this does `vmap[py_type] = visitor`.
        If only `py_type` is passed, it assumes you are decorating a
        visitor function definition:
          @vmap.register(some_type)
          def visit_some_type(o, w):
              ...
        """
        if visitor:
            self[py_type] = visitor
        else:
            def decorator(f):
                self[py_type] = f
                return f
            return decorator

class DEFAULT:
    ">>> visitor_map[DEFAULT] = visitor # sets default fallback visitor"

class _VisitorMapContextManager(object):
    """The `with` statement context manager returned by
    VisitorMap.as_context()"""
    def __init__(self, vmap, walker, set_parent_map=True):
        self.vmap = vmap
        self.original_map = None
        self.walker = walker
        self.set_parent_map = set_parent_map

    def __enter__(self):
        self.original_map = self.walker.visitor_map
        if self.set_parent_map:
            assert not self.vmap.parent_map
            self.vmap.parent_map = self.original_map
        self.walker.visitor_map = self.vmap

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.walker.visitor_map = self.original_map
        if self.set_parent_map:
            self.vmap.parent_map = None

################################################################################
# 4:  Default serialization visitors for standard Python types

# visitor signature = "f(obj_to_be_walked, walker)", return value ignored
# o = obj_to_be_walked, w = walker (aka serializer)
default_visitors_map = VisitorMap({
    str: (lambda o,w: w.walk(unicode(o, w.input_encoding, 'strict'))),
    unicode: (lambda o, w: w.emit(o)),
    safe_bytes: (lambda o, w: w.emit(unicode(o, w.input_encoding, 'strict'))),
    safe_unicode: (lambda o, w: w.emit(o)),
    types.NoneType: (lambda o, w: None),
    bool: (lambda o, w: w.emit(str(o))),
    type: (lambda o, w: w.walk(unicode(o))),
    DEFAULT: (lambda o, w: w.walk(repr(o)))})

number_types = (int, long, Decimal, float, complex)
func_types = (types.FunctionType, types.BuiltinMethodType, types.MethodType)
sequence_types = (tuple, list, set, frozenset, xrange, types.GeneratorType)

for typeset, visitor in (
    (number_types, (lambda o, w: w.emit(str(o)))),
    (sequence_types, (lambda o, w: [w.walk(i) for i in o])),
    (func_types, (lambda o, w: w.walk(o())))):
    for type_ in typeset:
        default_visitors_map[type_] = visitor


################################################################################
# 5: Declarative classes for creating a dom-like tree of xml/html elements:

# If you don't like this particular tag building syntax, please
# remember my main argument and use your imagination to dream up
# a better syntax.  The code above (sections 1-4) is what counts.
# Everything that follows can be swapped out.

class XmlName(safe_unicode):
    """An XML element or attribute name"""

class XmlAttributes(list): pass
class XmlAttribute(object):
    def __init__(self, value, name=None):
        self.value = value
        self.name = name

class XmlElement(object):
    attrs = None
    children = None

    def __init__(self, name):
        self.name = name

    def __call__(self, class_=None, **attrs):
        assert not self.attrs
        if class_ is not None:
            attrs['class'] = class_
        self.attrs = self._normalize_attrs(attrs)
        return self

    def _normalize_attrs(self, attrs):
        out = XmlAttributes()
        for n, v in attrs.items():
            if n.endswith('_'):
                n = n[:-1]
            if '_' in n:
                if '__' in n:
                    n = n.replace('__',':')
                elif 'http_' in n:
                    n = n.replace('http_', 'http-')
            # may eventually run into encoding issues with name:
            out.append(XmlAttribute(value=v, name=XmlName(n)))
        return out

    def _add_children(self, children):
        assert not self.children
        self.children = []
        if isinstance(children, (tuple, list)):
            self.children.extend(children)
        else:
            self.children.append(children)

    def __getitem__(self, children):
        self._add_children(children)
        return self

class XmlElementProto(object):
    def __init__(self, name, can_be_empty=False, element_class=XmlElement):
        self.name = XmlName(name)
        self.can_be_empty = can_be_empty
        self.element_class = element_class

    def __call__(self, class_=None, **attrs):
        if class_ is not None:
            attrs['class'] = class_
        return self.element_class(self.name)(**attrs)

    def __getitem__(self, children):
        return self.element_class(self.name)[children]

class XmlEntityRef(object):
    def __init__(self, alpha, num, description):
        self.alpha, self.num, self.description = (alpha, num, description)

class XmlCData(object):
    def __init__(self, content):
        self.content = content

class Comment(object):
    def __init__(self, content):
        self.content = content

class Script(XmlElement):
    pass

# This list of html tags isn't exhaustive.  It's just an example.
# The definitive list of tags and whether they can be empty is html
# version specific.  If you care about that, you could create a
# separate list for each html version.
_non_empty_html_tags = '''
  a abbr acronym address applet b bdo big blockquote body button
  caption center cite code colgroup dd dfn div dl dt em fieldset font
  form frameset h1 h2 h3 h4 h5 h6 head html i iframe ins kbd label
  legend li menu noframes noscript ol optgroup option pre q s samp
  select small span strike strong style sub sup table tbody td
  textarea tfoot th thead title tr tt u ul var'''.split()

_maybe_empty_html_tags = '''
    area base br col frame hr img input link meta p param script'''.split()

htmltags = dict(
    [(n, XmlElementProto(n, False)) for n in _non_empty_html_tags]
    + [(n, XmlElementProto(n, True)) for n in _maybe_empty_html_tags]
    + [('script', XmlElementProto('script', element_class=Script))])

# I have a separate module that defines the html entity refs.  Email
# me if you would like a copy.

################################################################################
# 6: Visitors for the xml/html elements, etc.

xml_default_visitors_map = default_visitors_map.copy()
# o = obj_to_be_walked, w = walker (aka serializer)
xml_default_visitors_map.update({
    unicode: (lambda o, w: w.emit(xml_escape(o))),
    XmlName: (lambda o, w: w.emit(unicode(o))),
    XmlAttributes: (lambda o, w: [w.walk(i) for i in o]),
    XmlElementProto: (lambda o, w: (
        w.emit(safe_unicode('<%s />'%o.name)
               if o.can_be_empty
               else safe_unicode('<%s></%s>'%(o.name, o.name))))),
    XmlEntityRef: (lambda o, w: w.emit(safe_unicode('&%s;'%o.alpha))),
    })

@xml_default_visitors_map.register(XmlElement)
def visit_xml_element(elem, walker):
    walker.emit_many(('<', elem.name))
    walker.walk(elem.attrs)
    walker.emit('>')
    walker.walk(elem.children)
    walker.emit('</%s>'%elem.name)

def _substring_replace_ctx(walker, s, r, ofilter=lambda x: x):
    return VisitorMap(
        {unicode: lambda o, w: w.emit(ofilter(o.replace(s, r, -1)))
         }).as_context(walker)

@xml_default_visitors_map.register(XmlAttribute)
def visit_xml_attribute(attr, walker):
    walker.emit_many((' ', attr.name, '="')) # attr.name isinstance of XmlName
    with _substring_replace_ctx(walker, '"', r'\"', xml_escape):
        walker.walk(attr.value)
    walker.emit('"')

@xml_default_visitors_map.register(Comment)
def visit_xml_comment(obj, walker):
    walker.emit('<!--')
    with _substring_replace_ctx(walker, '--','-/-'):
        walker.walk(obj.content)
    walker.emit('-->')

@xml_default_visitors_map.register(XmlCData)
def visit_xml_cdata(obj, walker):
    walker.emit('<![CDATA[')
    with _substring_replace_ctx(walker, ']]>',']-]->'):
        walker.walk(obj.content)
    walker.emit(']]>')

@xml_default_visitors_map.register(Script)
def visit_script_tag(elem, walker):
    walker.emit_many(('<', elem.name))
    walker.walk(elem.attrs)
    walker.emit('>')
    if elem.children:
        walker.emit('\n//')
        walker.walk(XmlCData(('\n', elem.children, '\n//')))
        walker.emit('\n')
    walker.emit('</%s>'%elem.name)
################################################################################
## End core module code, begin examples
################################################################################

################################################################################
# 7: Helpers for examples:

examples_vmap = xml_default_visitors_map.copy()

@examples_vmap.register(XmlElement)
def pprint_visit_xml_element(elem, walker):
    visit_xml_element(elem, walker)
    walker.emit('\n') # easier to read example output

class Example(object):
    all_examples = [] #class attr
    def __init__(self, name, content,
                 visitor_map=examples_vmap,
                 input_encoding='utf-8'):
        self.name = name
        self.content = content
        self.visitor_map = visitor_map
        self.input_encoding = input_encoding
        Example.all_examples.append(self)

    def show(self):
        print '-'*80
        print '## Output from example:', self.name
        print
        output = Serializer(
            self.visitor_map,
            self.input_encoding).serialize(self.content)
        print output.encode(get_default_encoding())


## put some html tags in the module scope to make the examples less
## verbose:
class _GetAttrDict(dict):
    def __getattr__(self, k):
        try:
            return self[k]
        except KeyError:
            raise AttributeError(k)
htmltags = _GetAttrDict(htmltags)
meta   = htmltags.meta
html   = htmltags.html
head   = htmltags.head
script = htmltags.script
title  = htmltags.title
body   = htmltags.body
div    = htmltags.div
span   = htmltags.span
h1     = htmltags.h1
h2     = htmltags.h2
ul     = htmltags.ul
li     = htmltags.li
## could also say:
#for k, v in htmltags.iteritems():
#    exec '%s = htmltags["%s"]'%(k, k)
## but then my pyflakes/flymake setup complains about undefined vars ...

################################################################################
# 8: Basic examples
Example(
    'Standard python types, no html',
    [1, 2, 3
     , 4.0
     , 'a', u'b'
     , ('c', ('d', 'e')
        , set(['f', 'f'])) # nested
     , (i*2 for i in xrange(10))
     ])
# output = '1234.0abcdef024681012141618'

Example(
    'Standard python types, no html *or* html escaping',
    [1, '<', 2, '<', 3],
    visitor_map=default_visitors_map)
# output = '1<2<3'

# To see output from the rest of the examples exec this module
Example(
    'Full html5 doc, no wrapper',
    [safe_unicode('<!DOCTYPE html>'),
     html(lang='en')[
         head[title['An example'], meta(charset='UTF-8')],
         body['Some content']
         ]
     ])

class HTML5Doc(object):
    def __init__(self, body, head=None):
        self.body = body
        self.head = (
            head if head
            else htmltags.head[title['An example'],
                               meta(charset='UTF-8')])

@examples_vmap.register(HTML5Doc)
def visit_html5_doc(doc, walker):
    walker.walk([safe_unicode('<!DOCTYPE html>'),
                 html(lang='en')[
                     doc.head,
                     doc.body]])

Example(
    'Full html5 doc, with wrapper',
    HTML5Doc(body('a_css_class')[div['content']]))

Example(
    'Full html5 doc, with wrapper and overriden head',
    HTML5Doc(body('wrapped')[div['content']],
             head=title['Overriden']))

Example(
    """Context-aware HTML escaping
    (does any template lang other than Genshi do this?)""",
    HTML5Doc(
        body(onload='func_with_esc_args(1, "bar")')[
            div['Escaped chars: ', '< ', u'>', '&'],
            script(type='text/javascript')[
                 'var lt_not_escaped = (1 < 2);',
                 '\nvar escaped_cdata_close = "]]>";',
                 '\nvar unescaped_ampersand = "&";'
                ],
            Comment('''
            not escaped "< & >"
            escaped: "-->"
            '''),
            div['some encoded bytes and the equivalent unicode:',
                '你好', unicode('你好', 'utf-8')],
            safe_unicode('<b>My surrounding b tags are not escaped</b>'),
            ]))

Example(
    'a snippet using a list comprehension',
    div[[span(id=('id', i))[i, ' is > ', i-1]
         for i in xrange(5)]])


################################################################################
# 9: Extended example using some fictional model data

from decimal import Decimal
class Money(Decimal):
    pass

class PriceRule(object):
    def __init__(self, oid, product, price):
        self.oid = oid
        self.product = product
        self.price = price

class PriceSet(list):
    pass

class Organization(object):
    def __init__(self, name):
        self.name = name
        self.price_rules = PriceSet()

######
# Imperative approach: simple, but inflexible

def render_org_prices__imperative(org):
    return (
        div[h1['Custom Prices For ', org.name],
            div[ul[(li[render_price(pr)] for pr in org.price_rules)]]]
        if org.price_rules
        else h1['No Custom Prices For ', org.name])

def render_price(pr):
    return span('price_rule', id=('rule', pr.oid))[
        pr.product, ': $%0.2f'%pr.price]

customer1 = Organization(name='Smith and Sons')
customer1.price_rules.extend(
    [PriceRule(oid=i, product='Product %i'%i, price=Money(str('%0.2f'%(i*1.5))))
     for i in xrange(10)])

Example(
    'Customer pricing printout, imperative',
    render_org_prices__imperative(customer1))

######
# Declarative approach: cleaner, modular and flexible
# Delegates as many choices as possible to visitors, with each visitor
# doing one thing only:

new_vmap = examples_vmap.copy()
class UIScreen(object):
    "Abstract declarations of ui screens"
    def __init__(self, title, content=None):
        self.title = title
        self.content = content

@new_vmap.register(UIScreen)
def visit_screen(screen, w):
    w.walk(HTML5Doc(
        body=body[h1[screen.title],
                  div('content')[screen.content]],
        head=head[title[screen.title]]))

@new_vmap.register(PriceSet)
def visit_priceset(pset, w):
    w.walk(ul[(li[pr] for pr in pset)])

@new_vmap.register(Money)
def visit_money(m, w):
    w.walk('$%0.2f'%m)

@new_vmap.register(PriceRule)
def visit_pricerule(pr, w):
    w.walk(span('price_rule', id=('rule', pr.oid))[pr.product, ': ', pr.price])

def render_org_prices__declarative(org):
    return UIScreen(
      title=('Custom Prices For ', org.name),
      content=(org.price_rules
               if org.price_rules
               else 'No custom prices assigned.'))
Example(
    'Customer pricing printout, declarative',
    render_org_prices__declarative(customer1),
    visitor_map=new_vmap)

################################################################################
if __name__ == '__main__':
    for example in Example.all_examples:
        example.show()
