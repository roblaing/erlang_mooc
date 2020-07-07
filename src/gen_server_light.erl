%% @doc Using gen_server conventions.
-module(gen_server_light).
-export([ start_link/2
        , stop/1
        , call/2
        , cast/2
        , init/2
        , loop/2
        ]).

start_link(Module, Args) ->
  register(Module, spawn_link(?MODULE, init, [Module, Args])).

stop(Name) -> 
  Name ! stop,
  stopped.

init(Module, Args) ->
  Init = Module:init(Args),
  loop(Module, Init).

loop(Module, State0) ->
  receive
    {call, From, Ref, Request} ->
      {reply, Reply, State1} = Module:handle_call(Request, From, State0),
      From ! {reply, Ref, Reply},
      loop(Module, State1);
    {cast, Request} -> 
      {noreply, State1} = Module:handle_cast(Request, State0),
      loop(Module, State1);
    stop -> Module:terminal(State0)
  end.  

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

%% @doc Asynchronous, does not need a reply from the server.
cast(RegName, Request) ->
  RegName ! {cast, Request},
  ok.

