%% @doc talk kills the echo process at N = 3. I found a 2 second pause is needed to avoid the repl hanging.
-module(talk).
-export([worker/0]).

worker() ->
  work(0).

work(N) ->
  case N of
    3 -> 
      % exit(whereis(echo), normal), % normal doesn't hang
      exit(whereis(echo), kill),
      timer:sleep(2000);
    _ -> true
  end,
  Msg = {self(), N},
  echo ! Msg,
  io:format("~w sent.~n", [Msg]),
  receive
    _Reply -> 
      timer:sleep(500),
      work(N+1)
  end.

