@doc Home page for my assignments for the University of Kent's Erlang courses offered via Futurelearn.

<h1>Pid Bang</h1>

Thinking in terms of sending and receiving messages rather than return values from functions with arguments I found a bit tricky.

Joe Armstrong in a this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

invokes the old Elvis song "Return to sender" as a handy reminder that 
<a href="https://erlang.org/doc/reference_manual/processes.html">processes</a> sending "arguments" (as messages) to
other processes need to provide a return address to get the result back in their mailbox. 
A process can simply give its return address as 
<a href="https://erlang.org/doc/man/erlang.html#self-0">self()</a>.

For a process created by <a href="https://erlang.org/doc/man/erlang.html#spawn-3">spawn(Module, Function, Args)</a>
to remain alive for repeated messages, it needs to be written as a <em>listening loop</em> that calls itself after handling a message.

Another tricky concept is the <a href="https://erlang.org/doc/reference_manual/expressions.html#receive">receive</a> 
block doesn't need to be in the process that makes the initial call, but can be in a subsidiary function.

<h2>Parallel Map</h2>

In this youtube video

<a href="https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2">
https://www.youtube.com/watch?v=0jsdXFUvQKE&amp;list=PLR812eVbehlwq4qbqswOWH7NLKjodnTIn&amp;index=2</a>

Joe Armstrong provides the following example (he leaves function F out of the argument list for pmap,
which I assume is a typo):

<code><pre>
function pmap(F, L) ->
  S = self(),
  Pids = [do(S, F) || F &lt;- L],
  [receive {Pid, Val} -> Val end || Pid &lt;- Pids].

do(Parent, F) ->
  spawn(fun() -> Parent ! {self(), F()} end).
</pre></code>

<h1>Error Handling</h1>

An important difference between Erlang and the Elvis song is if the <em>to address</em> is wrong
you won't get a bounceback saying <q>... address unknown. No such number, no such zone</q>, 
so care needs to be taken to avoid waiting at the mailbox for a response that will never come.

<h1>Signal vs Message?</h1>

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

<h1>Websocket</h1>

Does trapping apply to websocket?


Principle of remote error handling.

Need at least two computers.

Same mechanism for N computers.

<h1>Linking processes</h1>

Call <code>link(Pid)</code> in one process to lin to the process Pid.

If on process fails, linked processes fail too and processes linked to those will also fail.


spawn_link

Need just two primitives

link(A, B)

https://s3.us-east-2.amazonaws.com/ferd.erlang-in-anger/text.v1.1.0.pdf


