@doc This has been created by copying overview.edoc to README.md. Running <code>make doc</code> creates index.html which looks better.

<h1>Pid Bang</h1>

Thinking in terms of processes sending and receiving messages rather than return values from functions with 
arguments I found a bit tricky.

Joe Armstrong in this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

invokes the old Elvis song <q><em>Return to sender</em></q> as a handy reminder that 
<a href="https://erlang.org/doc/reference_manual/processes.html">processes</a> sending "arguments" (as messages) to
other processes need to provide a return address to get the result back in their mailbox. 
A process can simply give its return address as 
<a href="https://erlang.org/doc/man/erlang.html#self-0">self()</a>.

An important difference between Erlang and the Elvis song is the <em>to address</em> won't tell you 
<q><em>... address unknown. No such number, no such zone</em></q> if there's a quarrel, 
requiring digressions into monitors and timeouts.

For a process created by <a href="https://erlang.org/doc/man/erlang.html#spawn-3">spawn(Module, Function, Args)</a>
to remain alive for repeated messages, it needs to be written as a <em>listening loop</em> that calls itself after handling a message.

<h1>Message idioms</h1>

There's a tendancy to tell Erlang novices in tutorials that message structure is arbitrary. 
I think this misleading since Erlang is very much in the "convention over configuration" school, and teaching 
these conventions from the outset would make things much easier down the line.

A symptom of not understanding the idioms was my frequency server example's listening loop got increasingly cluttered 
with stanzas since I was writing separate ones for allocate, deallocate, inject...

The code I wrote while learning this is at <a href="https://github.com/roblaing/erlang_mooc/blob/master/src/gen_server_light.erl">
gen_server_light.erl</a>, which tries to be compatible with 
<a href="https://erlang.org/doc/design_principles/gen_server_concepts.html">gen_server</a> since I don't find it easy to understand.

<b>Something that tripped me up a few times with gen_server is tuple arguments. For instance, the first argument of
<code>start_link/4, ServerName</code>, should be <code>{local, RegName}</code>, the <code>From</code> 
argument in handle_call is a tuple <code>{pid(), reference()}</code>, and the return value from 
<code>Module:init(Arg)</code> needs to be <code>{ok, State}</code>.</b>

Standardising the message expected by the server loop to 

<code>{call, From, Ref, Request}</code> 

and its response to 

<code>{reply, Ref, Reply}</code>

or alternatively sending 

<code>{cast, Request}</code> 

when no response from the server is required, simplified my <code>loop(State)</code> function to:

```erlang
loop(Module, State0) ->
  receive
    {call, Pid, Ref, Request} ->
      {reply, Reply, State1} = Module:handle_call(Request, {Pid, Ref}, State0),
      Pid ! {reply, Ref, Reply},
      loop(Module, State1);
    {cast, Request} -> 
      {noreply, State1} = Module:handle_cast(Request, State0),
      loop(Module, State1);
    stop -> Module:terminate(normal, State0);
    Unknown -> % includes {'EXIT', Pid, Reason} when process_flag(trap_exit, true) has been set in Module:init(Init).
      {noreply, State1} = Module:handle_info(Unknown, State0),
      loop(Module, State1)
  end.  
```

This is mainly for educational purposes since OTP applications have builtin loop functions. Similarly, 
I've written my own versions of <code>call</code> and <code>cast</code> for learning purposes, 
sticking to the arguments and reply conventions of
<a href="https://erlang.org/doc/man/gen_server.html#call-2">gen_server:call(ServerRef, Request) -> Reply</a> and 
<a href="https://erlang.org/doc/man/gen_server.html#cast-2">gen_server:cast(ServerRef, Request) -> ok</a>.

What <code>call</code> and <code>cast</code> let us do is create a simple, standard template for client functions:

```erlang
allocate()       -> call(frequency, allocate).
deallocate(Freq) -> call(frequency, {deallocate, Freq}).
inject(Freqs)    -> cast(frequency, {inject, Freqs}).
...
```

with each of the above having an associated <code>handle_call/3</code> or <code>handle_cast/2</code> function which
I'll get to shortly.

My version of <code>call/2</code> looks like:

```erlang
call(RegName, Request) ->
  case whereis(RegName) of
    undefined -> {error, server_down}; % Needed for when server is not running in first place.
    Pid ->    
      Ref = monitor(process, Pid),
      RegName ! {call, self(), Ref, Request},
      receive
        {reply, Ref, Reply} -> 
          demonitor(Ref, [flush]), 
          Reply;
        {'DOWN', Ref, process, Pid, Info} -> % Server crashed after message sent.
          io:format("Pid ~p Info ~p", [Pid, Info]),
          {error, server_down};
        Unknown ->
          io:format("Don't know what to do with ~p~n", [Unknown])
        after 5000 -> exit(timeout)
      end
  end.
```

By default, gen_server:call/2 exits with a timeout failure after five seconds. Longer (or shorter) timeouts can be set
using <a href="https://erlang.org/doc/man/gen_server.html#call-3">call(ServerRef, Request, Timeout) -> Reply</a>.

After viewing 

<a href="https://www.youtube.com/watch?v=upGZMJBh81A&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc&amp;index=2">
https://www.youtube.com/watch?v=upGZMJBh81A&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc&amp;index=2</a>

I used <a href="https://erlang.org/doc/man/erlang.html#monitor-2">
monitor(Type :: process, Item :: monitor_process_identifier()) -> MonitorRef</a>
and <a href="https://erlang.org/doc/man/erlang.html#demonitor-1">demonitor(MonitorRef) -> true</a> to avoid a
call waiting for eternity for a reply from a dead server.

If the server crashes before responding, call would receive a message like

<code>{'DOWN',#Ref&lt;0.1606639298.2877292545.87648>,process,&lt;0.82.0>,normal}</code>

My cast looks like:

```erlang
cast(RegName, Request) ->
  RegName ! {cast, Request},
  ok.
```

It returns ok irrespective of whether the server received and handled the message or not.

<h2>Behaving yourself</h2>

Though we typically use pre-prepared behaviours, as a learning exercise I wanted to turn my
<a href="https://github.com/roblaing/erlang_mooc/blob/master/src/gen_server_light.erl">
gen_server_light.erl</a> into a something clients could address as <code>-behaviour(gen_server_light)</code>.

There appears to be an old and new way to do this. The old way, touched on by Professor Thompson, is to export
behaviour_info/1 and then write something like:

```erlang
behaviour_info(callbacks) ->
  [ {handle_call, 3}
  , {handle_cast, 2}
  , {handle_info, 2}
  , {init, 1}
  , {terminate, 1}
  ];
behaviour_info(_) -> undefined.
```

ie list functions the behaviour module expects to find in the client's code.

The <em>new way</em> appears to be to use the <code>-callback</code>
<a href="https://erlang.org/doc/reference_manual/modules.html#module-attributes">module attribute</a> which via
a google search I found described at 
<a href="http://davekuhlman.org/implement-a-behavior.html">http://davekuhlman.org/implement-a-behavior.html</a>.

The syntax for <code>-callback</code> is the same as <code>-spec</code>. 

```erlang
-callback handle_call(Request::term(), {From::pid(), Ref::reference()}, State::term()) -> 
  Result::{reply, Reply::term(), NewState::term()}.
-callback handle_cast(Request::term(), State::term()) -> Result::{noreply, NewState::term()}.
-callback handle_info(Info::term(), State::term()) -> Result::{noreply, NewState::term()}.
-callback init(Args::term()) -> Result::{ok, State::term()}.
-callback terminate(Reason::term(), State::term()) -> none().
```

This seems to work, though when I use <code>-behaviour(gen_server_light).</code> in the client module, I get a compiler
warning <q>Warning: behaviour gen_server_light undefined</q>, so something appears to be not quite right.

The limited documentation says: <q>The -callback attribute is to be preferred since the extra type information 
can be used by tools to produce documentation or find discrepancies.</q>

<h2>Client code</h2>

The key thing we do need to understand is how to write our own
<a href="https://erlang.org/doc/man/gen_server.html#Module:handle_call-3">Module:handle_call(Request, From, State) -> Result</a>
functions which return <code>{reply, Reply, NewState}</code>.

Note the second argument <code>From</code> is a tuple <code>{Pid, Ref}</code>. This tripped me up rewriting the code.

```erlang
handle_call(allocate, _, {[], Allocated}) -> 
  {reply, {error, no_frequency}, {[], Allocated}};
handle_call(allocate, {Pid, _}, {[Freq|Free], Allocated}) ->
  link(Pid),
  {reply, {ok, Freq}, {Free, [{Freq, Pid}|Allocated]}};

%% @doc Assumes deallocate request comes from the associated Pid. Could add another error message for when that's not the case.
handle_call({deallocate, Freq}, {Pid, _Ref}, {Free, Allocated}) ->
  case lists:member({Freq, Pid}, Allocated) of
    true  ->
      unlink(Pid),
      {reply, ok, {[Freq|Free], lists:delete({Freq, Pid}, Allocated)}};
    false ->
      io:format("Unallocated frequency ~p~n", [Freq]),
      {reply, {error, unallocated}, {Free, Allocated}}
  end;

handle_call(free, _From, {Free, Allocated}) ->
   {reply, length(Free), {Free, Allocated}};

handle_call(report, _From, {Free, Allocated}) ->
  io:format("Free: ~p Allocated ~p~n", [length(Free), length(Allocated)]),
  {reply, ok, {Free, Allocated}}.
```

When we don't need a response from the server, we use 
<a href="https://erlang.org/doc/man/gen_server.html#Module:handle_cast-2">Module:handle_cast(Request, State) -> Result</a>
which returns <code>{noreply, NewState}</code>.

```erlang
handle_cast({inject, Freqs}, {Free, Allocated}) ->
  {noreply, {Free ++ Freqs, Allocated}}.
```

Besides handle_call and handle_case, there's
<a href="https://erlang.org/doc/man/gen_server.html#Module:handle_info-2">Module:handle_info(Info, State) -> Result</a>
which seems specifically designed to respond to timeouts and exit messages.

To trap exits with gen_server, I modified the client init/1 function to:

```erlang
init(Init) ->
  process_flag(trap_exit, true),
  {ok, Init}.
```

and my handle_info looks like:

```erlang
handle_info({'EXIT', Pid, Reason}, {Free, Allocated}) ->
  io:format("~p exited: ~p~n", [Pid, Reason]),
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} -> 
      {noreply, {[Freq|Free], proplists:delete(Freq, Allocated)}};
    false       -> 
      {noreply, {Free, Allocated}}
  end;

handle_info(Info, State) ->
  io:format("Received unknown message ~p~n", [Info]),
  {noreply, State}.
```

<h1><a href="https://erlang.org/doc/reference_manual/errors.html">Error Handling</a></h1>

Erlang broadly has two ways of handling crashes: exceptions suited for traditional, sequential functions using 
<code>try ... catch ... end</code>, and exit signals for concurrent processes with <code>repeat ... end</code>.

<h2><a href="https://erlang.org/doc/reference_manual/processes.html#errors">Error propogation in a network of nodes</a></h2>

The key BIFs here are <a href="https://erlang.org/doc/man/erlang.html#link-1">link(PidOrPort) -> true</a> which links the
calling process to another, or 
<a href="https://erlang.org/doc/man/erlang.html#spawn_link-3">spawn_link(Module, Function, Args) -> pid()</a>
which is safer when calling spawn and then link since it does both as an atomic operation, avoiding the possibility of
linking to a dead process.

A node can kill itself with <a href="https://erlang.org/doc/man/erlang.html#exit-1">exit(Reason) -> no_return()</a>, and nodes
can be killed externally by <a href="https://erlang.org/doc/man/erlang.html#exit-2">exit(Pid, Reason) -> true</a>

A gotcha is two <em>exit reasons</em>, <code>normal</code> and <code>kill</code>, behave differently from <code>timeout</code>, 
  <code>shutdown</code> or whatever.

<dl>
  <dt>timeout, shutdown, ... whatever</dt><dd>If the linked process has
    <a href="https://erlang.org/doc/man/erlang.html#process_flag-2">process_flag(trap_exit, true)</a>,
    it will receive a message <code>{'EXIT', FromPid, Reason}</code>, otherwise it will call
    <code>exit(Reason)</code> for processes linked to it in turn to either trap, or exit and pass on to their links.</dd>
  <dt>kill</dt><dd>This is <em>untrappable</em>, meaning <code>kill</code> causes directly linked nodes to die even if they 
    have trap_exit set to true. Indirectly linked nodes trapping exits receive <code>{'EXIT', FromPid, killed}</code> 
    (note <em>kill</em> is rewritten as <em>killed</em>) so that they don't necessarily die.</dd>
  <dt>normal</dt><dd>Only <em>abnormal</em>, ie Reason is anything but <code>normal</code>, cause linked nodes to call
    <code>exit(Reason)</code> to pass on. A supervisor or other directly linked node, however, would receive a 
    <code>{'EXIT', FromPid, normal}</code> message.</dd>
</dl>

<h2>Sequential exception handling</h2>

Usually, errors are handled by sending either <code>{ok, Value}</code> or <code>{error, Reason}</code> messages.

When something unforeseen happens, a system <em>raises and exception</em>, commonly known as crashing.

Only use these to exit deeply nested recursion as in parsers.

There are three types of exceptions, each of which creates a different Class for catch:

```erlang
try Expr
catch
    throw:Term -> Term;
    exit:Reason -> {'EXIT', Reason}
    error:Reason:Stk -> {'EXIT', {Reason, Stk}}
end
```

<dl>
  <dt><a href="https://erlang.org/doc/man/erlang.html#exit-2">exit(Pid, Reason) -> true</a> or 
      <a href="https://erlang.org/doc/man/erlang.html#exit-1">exit(Reason) -> no_return()</a>
  </dt>
  <dd>Used with <a href="https://erlang.org/doc/man/erlang.html#link-1">link(PidOrPort) -> true</a>
      and <a href="https://erlang.org/doc/man/erlang.html#process_flag-2">process_flag(trap_exit, true)</a>
      to convert exit signals into <code>{'ERROR', From, Reason}</code> message.
  </dd>
  <dt><a href="https://erlang.org/doc/man/erlang.html#throw-1">throw(Any) -> no_return()</a></dt>
  <dd>Used with <a href="https://erlang.org/doc/reference_manual/expressions.html#catch-and-throw">catch</a> expressions
      which can be enhanced with <a href="https://erlang.org/doc/reference_manual/expressions.html#try">try</a>
  </dd>
  <dt><a href="https://erlang.org/doc/man/erlang.html#error-1">error(Reason) -> no_return()</a></dt>
  <dd>Includes stack trace for debugging</dd>
</dl>

<code><pre>
eval(Env, {div, Num, Denom}) ->
  N = eval(Env, Num),
  D = eval(Env, Denom),
  case D of
    0   -> throw(div_by_zero);
    _NZ -> N div D
  end;

try eval(Env, Exp) of
  Res -> Res
catch
  throw:div_by_zero -> 0
end
</pre></code>


<h2>Parallel Map</h2>

In this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

Joe Armstrong's provides an example which I've modified since he left function F out of the argument list for pmap,
which I assume is a typo, to look like this:

```erlang
pmap(F, Xs) ->
  S = self(),
  Pids = [do(S, F, X) || X &lt;- Xs],
  [receive {Pid, Val} -> Val end || Pid &lt;- Pids].

do(Parent, F, X) ->
  spawn(fun() -> Parent ! {self(), F(X)} end).
```

<h2>Futures</h2>

Another tricky concept is the <a href="https://erlang.org/doc/reference_manual/expressions.html#receive">receive</a> 
block doesn't need to be in the function that makes the initial call, but can be in a subsidiary function.

This means <code>call/2</code> 

```erlang
call(RegName, Request) ->
  Ref = monitor(process, whereis(RegName)),
  RegName ! {call, self(), Ref, Request},
  receive
    {reply, Ref, Reply} -> 
      demonitor(Ref, [flush]), 
      Reply;
    {'DOWN', Ref, process, Pid, Info} ->
      io:format("Pid ~p Info ~p", [Pid, Info]),
      {error, server_down};
    Unknown ->
      io:format("Don't know what to do with ~p~n", [Unknown])
  end.
```

can be split into two to look like what JavaScript etc call <code>futures</code>:

```erlang
promise(RegName, Request) ->
  Ref = monitor(process, whereis(RegName)),
  RegName ! {call, self(), Ref, Request},
  Ref.

yield(Ref) ->
  receive
    {reply, Ref, Reply} -> 
      demonitor(Ref, [flush]), 
      Reply;
    {'DOWN', Ref, process, Pid, Info} ->
      io:format("Pid ~p Info ~p", [Pid, Info]),
      {error, server_down};
    Unknown ->
      io:format("Don't know what to do with ~p~n", [Unknown])
  end.
```

<h1>Common problems</h1>

<h2>Race conditions</h2>

No guarantees about ordering (except point to point).


https://s3.us-east-2.amazonaws.com/ferd.erlang-in-anger/text.v1.1.0.pdf


http://erlang.org/doc/apps/observer/observer_ug.html

https://github.com/RefactoringTools/percept2

https://concuerror.com/

https://dl.acm.org/doi/10.1145/1596550.1596574

https://www.cs.kent.ac.uk/projects/wrangler/Wrangler/Home.html

<h1><a href="https://erlang.org/doc/reference_manual/distributed.html">Distributed</a></h1>

<h1>OTP</h1>

https://www.youtube.com/watch?v=9HVvzSsdW9k&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc


https://www.youtube.com/watch?v=YaUPdgtUYko
