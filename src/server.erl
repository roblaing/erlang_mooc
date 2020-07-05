-module(server).
-export([ start/2
        , stop/1
        , request/2
        , init/2
        ]).

start(Name, Args) ->
  register(Name, spawn(server, init, [Name, Args])).

stop(Name) ->
  Name ! stop.

init(Module, Args) ->
  Init = Module:init(Args),
  loop(Module, Init).

request(Name, Msg) ->
  Ref = make_ref(),
  Name ! {request, {self(), Ref}, Msg},
  receive
    {reply, Ref, Reply} -> Reply
  end.

loop(Module, State0) ->
  receive
    {request, {From, Ref}, Msg} ->
      {Reply, State1} = Module:handle(Msg, State0),
      From ! {reply, Ref, Reply},
      loop(Module, State1);
    stop ->
      Module:terminate(State0)
  end.
