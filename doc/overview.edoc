@doc Home page for my assignments for the University of Kent's Erlang courses offered via Futurelearn.

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

<h1>Linking processes</h1>

Call <code>link(Pid)</code> in one process to lin to the process Pid.

If on process fails, linked processes fail too and processes linked to those will also fail.


spawn_link

Need just two primitives

link(A, B)


<h2>Signal vs Message?</h2>

Signals are not messages in Erlang

When a process terminates abnormally, it sends a signal to all the processes linked to it, with the reason killed.

Signals propogate immediately, and the default behaviour on receiving a signal is to terminate (abnormally) yourself.

Signals and messages are different, but they are connected.

If we're able to deal with a process failing, we need to know when a process has failed, without being killed ourselves.

process_flag(trap_exit, true) to turn some processes into system processes

then exit signals to that process are converted to messages:

{'EXIT', FromPid, Reason}

<h2>Sending exit signals</h2>

Exits happen for all sorts of reasons: eg division by 0, ...

Can be triggered in a process itself by calling exit(Reason).

Can be caused by another process calling exit(Pid, Reason).

Normal termination has the reason normal, any other reason is abnormal.

Dealing with abnormal termination

When a process terminates abnormally, it sends a signal to all the processes linked to it.

<h2>Supervisors</h2>

Supervisors <em>spawn</em> workers.




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


