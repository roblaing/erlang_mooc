<h1>Erlang Mooc Notes</h1>

This is space for my assignments and notes for two online courses on Erlang offered by University of Kent professor Simon Thompson,
who is also co-author of O'Reilly's Erlang Programming book which the courses complement nicely.

<ol>
  <li><a href="https://www.futurelearn.com/courses/functional-programming-erlang">Functional Programming in Erlang</a></li>
  <li><a href="https://www.futurelearn.com/courses/concurrent-programming-erlang">Concurrent Programming in Erlang</a></li>
</ol>

Something I got converted to by an online course on open-source Lisp-dialect Racket using the free and excellent
<a href="https://htdp.org">How to Design Programs</a> (HTDP) textbook is its <a href="https://htdp.org/2020-5-6/Book/part_preface.html#%28part._sec~3asystematic-design%29">six step recipe</a> which I've used to structure this README below.

Erlang has excellent tools for all this &mdash; probably more so than Racket which doesn't encourage modularisation much &mdash;
but one criticism I have of Professor Thompson's course is while he touches on specifications, documentation, and testing, he
doesn't give them the supremacy I feel they deserve.

To train myself in Erlang while developing the good habits of the HTDP school,
I structured my <a href="https://erlang.org/doc/design_principles/applications.html#directory-structure">subdirectories</a>
roughly according to the recommendations:

<code><pre>
─ myproject
  ├── Makefile
  ├── doc
  │   └── overview.edoc
  ├── ebin
  ├── priv
  ├── src
  └── test
</pre></code> 

I've created my own small <a href="https://www.gnu.org/software/make/manual/make.html">Makefile</a> to keep things simpler than
<a href="https://www.rebar3.org/">rebar3</a> or <a href="https://erlang.mk/">erlang.mk</a>.

Running <code>make</code> will compile <code>src/foo.erl</code> into <code>ebin/foo.beam</code>.

Running <code>make doc</code> will use <a href="http://erlang.org/doc/apps/edoc/chapter.html">EDoc</a> to create
<code>doc/index.html</code> and related content which can be viewed by pointing your browser there.

Running <code>make test</code> will use <a href="https://erlang.org/doc/apps/common_test/introduction.html">Common Test</a>
to run <code>test/foo_SUITE.erl</code> files whose results you can view by pointing your browser to <code>test/index.html</code>. 

Running <code>erl -pa ebin</code> makes the repl aware your code in the ebin subdirectory.

<h2>1. From Problem Analysis to Data Definitions</h2>

<q>Identify the information that must be represented and how it is represented in the chosen programming language. 
Formulate data definitions and illustrate them with examples.</q>

The general advice for the first step is to step away from your computer and design with a pencil and paper. As someone more
comfortable with a keyboard who can't read his own handwriting, I find working with
<a href="https://graphviz.org/">graphviz</a>, <a href="https://d3js.org/">D3</a>, or simply waffling away in a text editor
(as I'm doing now) easier.

The key point is, the old cliché "The sooner you start to code, the longer the program will take" stands. As IBM liked to tell
my dad via a paper calendar, "THINK".

<h2>2. Signature, Purpose Statement, Header</h2>

A reason the HTDP recipe bundles these three together is they need to be done concurrently. Since the guts of a program
is likely to change often as you get more experienced with a given language &mdash; with your code hopefully getting
shorter and faster as your knowledge grows &mdash; it's vital to focus on "what" rather than "how" first. 

<h3>2.1 Signature</h3>

<q>State what kind of data the desired function consumes and produces.</q>

What HTDP terms <em>signatures</em> are also commonly known as <em>APIs</em>, <em>contracts</em>, 
or <em>specifications</em>, the jargon
used in Erlang's <a href="https://erlang.org/doc/reference_manual/typespec.html">
<code>-spec</code> and <code>-type</code></a> annotations.

Here we think in terms of <em>sets</em> (the basic building blocks, ie <em>atoms</em>, of mathematics, 
as explained in a brilliant lesson by <a href="https://www.youtube.com/watch?v=JsduHKckB04">Eddie Woo</a>), 
or <em>types</em> as sets are known in computer science, and operations on these sets.

Erlang promotes this thinking with its <a href="http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html">dialyzer</a> and
<a href="http://erlang.org/doc/man/typer.html">typer</a> tools.

<em>Tip: use <code>typer foo.erl</code> to check your <code>-spec ...</code> statements by commenting them out after you've written them. Regard it as a testing tool, not a substitute for thinking.</em>

As does any programming language, Erlang has a 
<a href="https://www.cs.tufts.edu/~nr/cs257/archive/barbara-liskov/data-abstraction-and-hierarchy.pdf">type hirarchy</a>, 
a phrase I think Turing-award winner Barbara Liskov coined.

I've diagramed Erlang's type hirarchy (work in progress) like this:

<code><pre>
any()
├── term()
│   ├── number()                   % union of integer() and float()
│   │   ├── integer()              % base#number eg 2:100 = 4   16#F09A29 = 15768105
│   │   │   ├── non_neg_integer()  % 0..
│   │   │   │   ├── pos_integer()  % 1..
│   │   │   │   ├── char()         % 33 (!) ... 255 (ÿ)
│   │   │   │   ├── byte()         % 0..255
│   │   │   │   └── arity()        % 0..255   bounded integer
│   │   │   └── neg_integer()      % ..-1
│   │   └── float()                % is_float(Term)
│   ├── atom()                     % is_atom(Term)
│   │   ├── boolean()              % false | true   bool() is a deprecated synonym
│   │   ├── module()
│   │   └── node()
│   ├── binary()                   % <a href="https://learnyousomeerlang.com/starting-out-for-real#bit-syntax">bit syntax</a>
│   │   └── bitstring()

├── Compound
│   ├── tuple()                    % {T1, T2, ..., TN}
│   │   └── mta()                  % {module(), atom(), arity()}
│   ├── list()                     % [any()]
│   │   ├── nil()                  % []
│   │   ├── nonempty_list()        % [T, ...]
│   │   ├── maybe_improper_list(Type1, Type2) % eg [3|3] is an improper list
│   │   ├── proplists:proplist()   % [atom() | tuple()]
│   │   └── string()               % [char()]
│   │       └── nonempty_string()
│   ├── array:array()
│   └── map()

├── Union
│   ├── timeout()                  % non_neg_integer() | infinity

├── pid()            % self().            
├── reference()      % erlang:make_ref().
├── port()
├── fun()            % fun((...) -> Type)
│   └── function()
├── ets:tid()
└── none()
    └── no_return()

    iodata()
    iolist()
</pre></code>

When it comes to the taxonomy of programming language, I find this <a href="https://www.famicol.in/language_checklist.html">
joke checklist</a> a handy reference. Erlang checks the box for <em>dynamically-typed</em> as opposed to <em>statically-typed</em>
(the creators of the list ommited <em>optionally-typed</em>), which means it has lots of <code>is_type(X)</code> built-in functions
(BIFs in Erlang jargon).

<h3>2.2 Purpose Statement</h3>

<q>Formulate a concise answer to the question <em>what</em> the function computes.</q>

In the case of <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a>, any comment like

<code><pre>
%% @doc Purpose statement goes here...
myfunc(Arg1,...) ->
  ...
</pre></code>

will then be included in the <em>Function Index</em> table and in the <em>Function Details</em> of the 
module's automatically generated html.

Besides being helpful to users, writing purpose statements also helps one think of 
suitable names for functions and their arguments to help make code self documenting.

<h3>2.3 Header</h3>

<q>Define a stub that lives up to the signature.</q>

The quip <q>fake it till you make it</q> is often used here. 
Without a stub to fool the compiler, we can't proceed with test-driven development.

All we want at this is stage is something with the same names and arities as our functions in our <em>wish list</em> whose
return values are of the correct type (typically hard-coded return values to give usually a wrong answer).

Warnings about variables in the header that are not used in the body are fine, in fact a handy reminder this is just work
in progress. 

<h2>3. Functional Examples</h2>

<q>Work through examples that illustrate the function’s purpose.</q>

The first thing I tend to look for in documentation when learning an unfamiliar function is a simple example of how to use it.

Besides creating good documentation, example-driven development (a phrase I prefer to test-driven development since you need
to think of examples before you can write tests) helps develop better final code.

One advantage of Erlang not being a <a href="https://www.tiobe.com/tiobe-index/">top of the pops progamming language</a>
is it doesn't suffer as much from the <em>too many tools</em> problem, but there is nevertheless plenty of confusion. 

I've used <a href="http://erlang.org/doc/apps/eunit/chapter.html">EUnit</a> rather than 
<a href="https://erlang.org/doc/apps/common_test/introduction.html">Common Test</a> in some cases since it's easier for
small projects.

In an ideal world, the test and document tools would be integrated. Edoc isn't linked to any test framework I know of, so it's good
practice to copy at least one illustrative example of each public function into its comment header for EDoc to put into the html.

<h2>4. Function Template</h2>

<q>Translate the data definitions into an outline of the function.</q>

Here we move from "what" to "how", exploring which architectural patterns fit the problem at hand.

Erlang's <a href="https://erlang.org/doc/design_principles/des_princ.html">OTP design principles</a> with their
<code>-behaviour(X)</code> attribute encourages good practice here.

A nice thing about Erlang is it encourges top-down design of big systems rather than the myopia of bottom-up design.

<h2>5. Function Definition</h2>

<q>Fill in the gaps in the function template. Exploit the purpose statement and the examples.</q>

<h2>6. Testing</h2>

<q>Articulate the examples as tests and ensure that the function passes all. Doing so discovers mistakes. 
Tests also supplement examples in that they help others read and understand the definition when the need 
arises—and it will arise for any serious program.</q>


