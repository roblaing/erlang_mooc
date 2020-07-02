%% @doc echo kills the talk process on N = 5, causing it to reset to 0.
-module(echo).
-export([listener/0]).

listener() ->
  receive
    {Pid, 5} -> 
      io:format("5 killed ~w.~n", [Pid]),
      exit(Pid, kill_worker),
      listener();
    {Pid, M} ->
      io:format("~w echoed.~n",[M]),
      Pid ! M,
      listener()
  end.


