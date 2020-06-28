<h1>Erlang Mooc Notes</h1>

This is space for my assignments and notes for two online courses on Erlang offered by University of Kent professor Simon Thompson,
who is also co-author of O'Reilly's Erlang Programming book which the courses complement nicely.

<ol>
  <li><a href="https://www.futurelearn.com/courses/functional-programming-erlang">Functional Programming in Erlang</a></li>
  <li><a href="https://www.futurelearn.com/courses/concurrent-programming-erlang">Concurrent Programming in Erlang</a></li>
</ol>

Something I got converted to by an online course on open-source Lisp-dialect Racket using the free and excellent
<a href="https://htdp.org">How to Design Programs</a> textbook with its <a href="https://htdp.org/2020-5-6/Book/part_preface.html#%28part._sec~3asystematic-design%29">six step recipe</a> is test-driven development &mdash;
a hard habit to develop unless started early &mdash; and generally 
<a href="https://lamport.azurewebsites.net/tla/tla.html">thinking above the code</a> (meta progamming, if you like jargon).

Erlang has excellent tools for all this &mdash; probably more so than Racket which doesn't encourage modularisation much &mdash;
but one criticism I have of Professor Thompson's course is while he touches on specifications, documentation, and testing, he
doesn't give them the supremacy I feel the deserve.

To train myself in Erlang concurrently (pun intended) with its specification, typing, documentation and various testing tools,
I structured my <a href="https://erlang.org/doc/design_principles/applications.html#directory-structure">subdirectories</a>
roughly according to the recommendations:

<code><pre>
─ <here>
  ├── Makefile
  ├── doc
  │   └── overview.edoc
  ├── ebin
  ├── priv
  ├── src
  └── test
</pre></code> 

I've created my own <a href="https://www.gnu.org/software/make/manual/make.html">Makefile</a> to sidestep the over-elaborations
or rebar3 and other Erlang third-party build tools.

Running <code>make</code> will compile <code>src/foo.erl</code> into <code>ebin/food.beam</code>.

Running <code>make doc</code> will use <a href="http://erlang.org/doc/apps/edoc/chapter.html">EDoc</a> to create
<code>doc/index.html</code> and related content which can be viewed by pointing your browser there.

Running <code>make test</code> will use <a href="https://erlang.org/doc/apps/common_test/introduction.html">Common Test</a>
to run <code>test/foo_SUITE.erl</code> files whose results you can view by pointint your browser to <code>test/index.html</code>. 

One advantage of Erlang not being a 
<a href="https://www.tiobe.com/tiobe-index/">top of the pops progamming language</a>
is it doesn't suffer as much from the <em>too many tools</em> problem, but there is nevertheless plenty of confusion. I've
used <a href="http://erlang.org/doc/apps/eunit/chapter.html">EUnit</a> rather than 
<a href="https://erlang.org/doc/apps/common_test/introduction.html">Common Test</a> in some cases since it's easier for
small projects.

<h2>1. From Problem Analysis to Data Definitions</h2>

The general advice for the first step is to step away from your computer and design with a pencil and paper. As someone more
comfortable with a keyboard who can't read his own handwriting, I find working with
<a href="https://graphviz.org/">graphviz</a>, <a href="https://d3js.org/">D3</a>, or simply waffling away in a text editor
(as I'm doing now) easier.

The key point is, the old cliché "The sooner you start to code, the longer the program will take" stands. As IBM liked to tell
my dad via a paper calendar, "THINK".

<h2>2. Signature, Purpose Statement, Header</h2>

A reason the HTDP recipe bundles these three together is they need to be done concurrently.

<h3>2.1 Signature</h3>

What HTDP terms <em>signatures</em> are also commonly known as <em>contracts</em>, or <em>specifications</em>, the jargon
used in Erlang's <a href="https://erlang.org/doc/reference_manual/typespec.html">
<code>-spec</code> and <code>-type</code></a> annotations.

A key part of this is thinking in terms of <em>sets</em> (as explained in the brilliant lesson by 
<a href="https://www.youtube.com/watch?v=JsduHKckB04">Eddie Woo</a>), or <em>types</em> as sets are known in computer science.

Erlang encourages this with its <a href="http://erlang.org/doc/apps/dialyzer/dialyzer_chapter.html">dialyzer</a> and
<a href="http://erlang.org/doc/man/typer.html">typer</a> tools.

<q>Tip: use <code>typer foo.erl</code> to check your <code>-spec ...</code> statements by commenting them out after you've written them. Regard it as a testing tool, not a substitute for thinking.</q>

As with any programming language, Erlang has a 
<a href="https://www.cs.tufts.edu/~nr/cs257/archive/barbara-liskov/data-abstraction-and-hierarchy.pdf">type hirarchy</a>, 
a phrase I think Turing-award winner Barbara Liskov coined.

My type hirarchy for Erlang (work in progress) looks like this:

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
│   │   ├── maybe_improper_list(Type1, Type2) % eg [3|3] an improper list means the second part is not a list.
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

A reason "serious" programmers hate dynamically-typed languages is that unless the author of a library deigned to throw users a frickin' bone by explaining what type the inputs and outputs of their functions are in their documentation, their work is unusable.

<h3>2.2 Purpose Statement</h3>

<h3>2.3 Header</h3>

<h2>3. Functional Examples</h2>

<h2>4. Function Template</h2>

<h2>5. Function Definition</h2>

<h2>6. Testing</h2>

