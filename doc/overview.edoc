@doc This has been created by copying overview.edoc to README.md. Running <code>make doc</code> creates index.html which looks better.

<h1>Pid Bang</h1>

Thinking in terms of sending and receiving messages between listening loops rather than return values from functions with 
arguments I found a bit tricky.

Joe Armstrong in this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

invokes the old Elvis song "Return to sender" as a handy reminder that 
<a href="https://erlang.org/doc/reference_manual/processes.html">processes</a> sending "arguments" (as messages) to
other processes need to provide a return address to get the result back in their mailbox. 
A process can simply give its return address as 
<a href="https://erlang.org/doc/man/erlang.html#self-0">self()</a>.

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

<h2>after Timeout -> ...</h2>

An important difference between Erlang and the Elvis song is if the <em>to address</em> is wrong
you won't get a bounceback saying <q>... address unknown. No such number, no such zone</q>, 
so care needs to be taken to avoid waiting at the mailbox for a response that will never come.

<h1><a href="https://erlang.org/doc/reference_manual/errors.html">Error Handling</a></h1>

This is a fairly complex topic, introducing several new primitives.

Erlang broadly has two ways of handling crashes: one suited for when the caller is a message handler with repeat ... end,
and another for traditional function calls using try ... catch ... end.

<h2>Error propogation in a network of nodes</h2>

The key BIFs here are <a href="https://erlang.org/doc/man/erlang.html#link-1">link(PidOrPort) -> true</a> which links the
calling process to another, or 
<a href="https://erlang.org/doc/man/erlang.html#spawn_link-3">spawn_link(Module, Function, Args) -> pid()</a>
which is safer when calling spawn since it does both as an atomic operation (avoding a race condition if the spawned process should
immediately die).

If the process spawning a child process is its <em>supervisor</em>, it needs to set
<a href="https://erlang.org/doc/man/erlang.html#process_flag-2">process_flag(trap_exit, true)</a>.

Otherwise, if <em>trap_exit</em> is left at the default <em>false</em>, linked processes are designed to fall like dominoes.

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


https://s3.us-east-2.amazonaws.com/ferd.erlang-in-anger/text.v1.1.0.pdf


http://erlang.org/doc/apps/observer/observer_ug.html

https://github.com/RefactoringTools/percept2

https://concuerror.com/

https://dl.acm.org/doi/10.1145/1596550.1596574


