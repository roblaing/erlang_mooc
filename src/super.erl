%% @doc Run test as super:super().
-module(super).
-export([super/0]).

super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo, listener,[]),
    register(echo,E),
    io:format("echo spawned as Pid ~w.~n", [whereis(echo)]),
    T = spawn_link(talk, worker,[]),
    register(talk,T),
    io:format("talk spawned as Pid ~w.~n", [whereis(talk)]),
    loop(E,T).

loop(E,T) ->
  receive
    {'EXIT', T, Reason} ->
      io:format("talk exited ~s~n", [Reason]),
      NewT = spawn_link(talk,worker,[]),
      register(talk,NewT),
      io:format("talk re-spawned as Pid ~w.~n",[whereis(talk)]),
      loop(E,NewT);
    {'EXIT', E, Reason} ->
      io:format("echo exited ~s~n", [Reason]),
      timer:sleep(1000), 
      NewE = spawn_link(echo,listener,[]),
      register(echo,NewE),
      io:format("echo re-spawned as Pid ~w.~n", [whereis(echo)]),
      loop(NewE,T)
  end.

