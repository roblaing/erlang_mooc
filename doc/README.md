@doc This has been created by copying overview.edoc to README.md. Running <code>make doc</code> creates index.html which looks better.

<h1>Pid Bang</h1>

Thinking in terms of sending and receiving messages between listening loops rather than return values from functions with 
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
<q><em>... address unknown. No such number, no such zone</em></q> by default, requiring digressions into monitors and timeouts.

For the frequency server example, I found my listening loop getting increasingly cluttered with stanzas since I was
writing separate ones for allocate, deallocate, inject...

Both in this course and various tutorials, learners get told messages structure is arbitrary. This is misleading
since Erlang is very much in the "convention over configuration school".

Standardising the message expected by the server loop to

<code>{request, From, Ref, Request}</code>

and its response to

<code>{reply, Ref, Reply}</code>

resulted in much simpler code.

Ignoring stop and exit handling, the <code>loop(State)</code> function simplifies to:

<code><pre>
loop(State0) ->
  receive
    {request, From, Ref, Request} ->
      {reply, Reply, State1} = handle_call(Request, From, State0),
      From ! {reply, Ref, Reply},
      loop(State1)
  end.  
</pre></code>

Though we don't use the <a href="https://erlang.org/doc/design_principles/gen_server_concepts.html">gen_server</a>
through most of the course, getting ready to replace our server code with the
provided module is an enlightening exercise on abstraction and "don't repeat yourself" (DRY) coding.

A function gen_server calls <a href="https://erlang.org/doc/man/gen_server.html#call-2">call(ServerRef, Request) -> Reply</a>,
creating a good convention. There is an alternative
<a href="https://erlang.org/doc/man/gen_server.html#call-3">call(ServerRef, Request, Timeout) -> Reply</a>, but I 
personally found timeouts confusing, prefering to use 
<a href="https://erlang.org/doc/man/erlang.html#monitor-2">
monitor(Type :: process, Item :: monitor_process_identifier()) -> MonitorRef</a>
and <a href="https://erlang.org/doc/man/erlang.html#demonitor-1">demonitor(MonitorRef) -> true</a> to avoid
call waiting for eternity for a reply from a dead server as explained in this video

<a href="https://www.youtube.com/watch?v=upGZMJBh81A&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc&amp;index=2">
https://www.youtube.com/watch?v=upGZMJBh81A&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc&amp;index=2</a>

What <code>call</code> lets us do is create a simple, standard template for client functions:

<code><pre>
allocate()       -> call(frequency, allocate).
deallocate(Freq) -> call(frequency, {deallocate, Freq}).
inject(Freqs)    -> call(frequency, {inject, Freqs}).
...
</pre></code>

My version of call looks like.

<code><pre>
call(RegName, Request) ->
  Ref = monitor(process, whereis(RegName)),
  RegName ! {request, self(), Ref, Request},
  receive
    {reply, Ref, Reply} -> 
      demonitor(Ref, [flush]), 
      Reply;
    {'DOWN', Ref, process, Pid, Info} ->
      io:format("Pid ~p Info ~p", [Pid, Info]),
      {error, server_down}
  end.
</pre></code>

If the server crashes before responding, call would receive a message like

<code>{'DOWN',#Ref&lt;0.1606639298.2877292545.87648>,process,&lt;0.82.0>,normal}</code>

In an OTP application, I'd simply use the provided call function instead of writing my own, and not bother about
how the library had implemented loop.

The key thing we do need to understand how to write our own
<a href="https://erlang.org/doc/man/gen_server.html#Module:handle_call-3">Module:handle_call(Request, From, State) -> Result</a>
functions.

<code><pre>
handle_call(allocate, _, {[], Allocated}) -> 
  {reply, {error, no_frequency}, {[], Allocated}};
handle_call(allocate, From, {[Freq|Free], Allocated}) ->
  link(From),
  {reply, {ok, Freq}, {Free, [{Freq, From}|Allocated]}};

handle_call({deallocate, Freq}, From, {Free, Allocated}) ->
  case lists:member({Freq, From}, Allocated) of
    true ->
      unlink(From),
      {reply, ok, {[Freq|Free], proplists:delete(Freq, Allocated)}};
    false ->
      io:format("Unallocated frequency ~p~n", [Freq]),
      {reply, {error, unallocated}, {Free, Allocated}}
  end;

handle_call({inject, Freqs}, _From, {Free, Allocated}) ->
  {reply, ok, {Free ++ Freqs, Allocated}};

handle_call(free, _From, {Free, Allocated}) ->
   {reply, length(Free), {Free, Allocated}}.
</pre></code>


https://erlang.org/doc/design_principles/gen_server_concepts.html

https://erlang.org/doc/man/gen_server.html


<h2>Parallel Map</h2>

In this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

Joe Armstrong provides the following example (he left function F out of the argument list for pmap,
which I assume is a typo):

<code><pre>
function pmap(F, L) ->
  S = self(),
  Pids = [do(S, F) || F &lt;- L],
  [receive {Pid, Val} -> Val end || Pid &lt;- Pids].

do(Parent, F) ->
  spawn(fun() -> Parent ! {self(), F()} end).
</pre></code>

For a process created by <a href="https://erlang.org/doc/man/erlang.html#spawn-3">spawn(Module, Function, Args)</a>
to remain alive for repeated messages, it needs to be written as a <em>listening loop</em> that calls itself after handling a message.

Another tricky concept is the <a href="https://erlang.org/doc/reference_manual/expressions.html#receive">receive</a> 
block doesn't need to be in the process that makes the initial call, but can be in a subsidiary function.



<h1><a href="https://erlang.org/doc/reference_manual/errors.html">Error Handling</a></h1>

This is a fairly complex topic, introducing several new primitives.

Erlang broadly has two ways of handling crashes: one suited for when the caller is a message handler with repeat ... end,
and another for traditional function calls using try ... catch ... end.

<h2><a href="https://erlang.org/doc/reference_manual/processes.html#errors">Error propogation in a network of nodes</a></h2>

The key BIFs here are <a href="https://erlang.org/doc/man/erlang.html#link-1">link(PidOrPort) -> true</a> which links the
calling process to another, or 
<a href="https://erlang.org/doc/man/erlang.html#spawn_link-3">spawn_link(Module, Function, Args) -> pid()</a>
which is safer when calling spawn and then link since it does both as an atomic operation, avoiding the possibility of
linking to a dead process.

If the process spawning a child process is its <em>supervisor</em>, it needs to set
<a href="https://erlang.org/doc/man/erlang.html#process_flag-2">process_flag(trap_exit, true)</a>.

Otherwise, if <em>trap_exit</em> is left at the default <em>false</em>, linked processes are designed to fall like dominoes,
unless the exit reason is <em>normal</em>.

A node can kill itself with <a href="https://erlang.org/doc/man/erlang.html#exit-1">exit(Reason) -> no_return()</a>, and nodes
can be killed externally by <a href="https://erlang.org/doc/man/erlang.html#exit-2">exit(Pid, Reason) -> true</a>

If the supervisor node has <em>trap_exit</em> set to <em>true</em>, instead of topling over it needs to handle a message
<code>{'EXIT', FromPid, Reason}</code>.

Reason can be any atom, but there's a little subtlety about <code>exit(kill)</code>: the linked process receives 
<code>{'EXIT', FromPid, killed}</code>

This doesn't seem to apply other exit reasons like <em>normal</em> etc.

<h2>Exceptions</h2>

Usually, errors are handled by sending either <code>{ok, Value}</code> or <code>{error, Reason}</code> messages.

Only use these to exit deeply nested recursion as in parsers.

There are three types of exceptions, each of which creates a different Class for catch:

<code><pre>
try Expr
catch
    throw:Term -> Term;
    exit:Reason -> {'EXIT', Reason}
    error:Reason:Stk -> {'EXIT', {Reason, Stk}}
end
</pre></code>

<dl>
  <dd><a href="https://erlang.org/doc/man/erlang.html#exit-2">exit(Pid, Reason) -> true</a> or 
      <a href="https://erlang.org/doc/man/erlang.html#exit-1">exit(Reason) -> no_return()</a>
  </dd>
  <dt>Used with <a href="https://erlang.org/doc/man/erlang.html#link-1">link(PidOrPort) -> true</a>
      and <a href="https://erlang.org/doc/man/erlang.html#process_flag-2">process_flag(trap_exit, true)</a>
      to convert exit signals into <code>{'ERROR', From, Reason}</code> message.
  </dt>
  <dd><a href="https://erlang.org/doc/man/erlang.html#throw-1">throw(Any) -> no_return()</a></dd>
  <dt>Used with <a href="https://erlang.org/doc/reference_manual/expressions.html#catch-and-throw">catch</a> expressions
      which can be enhanced with <a href="https://erlang.org/doc/reference_manual/expressions.html#try">try</a>
  </dt>
  <dd><a href="https://erlang.org/doc/man/erlang.html#error-1">error(Reason) -> no_return()</a></dd>
  <dt>Includes stack trace for debugging</dt>
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

<h1>Common problems</h1>

<h2>Race conditions</h2>

No guarantees about ordering (except point to point).


https://s3.us-east-2.amazonaws.com/ferd.erlang-in-anger/text.v1.1.0.pdf


http://erlang.org/doc/apps/observer/observer_ug.html

https://github.com/RefactoringTools/percept2

https://concuerror.com/

https://dl.acm.org/doi/10.1145/1596550.1596574

<h1><a href="https://erlang.org/doc/reference_manual/distributed.html">Distributed</a></h1>

<code></code>

<h1>OTP</h1>

https://www.youtube.com/watch?v=9HVvzSsdW9k&amp;list=PLR812eVbehlx6vgWGf2FLHjkksAEDmFjc


