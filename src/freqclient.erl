%% @doc This randomly tests the frequency server.
%%
%% <code>freqclient:random_test(1000).</code>
-module(freqclient).
-export([ client/0
        , add_client/0
        , delete_client/1
        , kill_client/1
        , random_test/1
        ]).

client() ->
  receive
    {From, call} -> 
      From ! frequency_gen:allocate(),
      client();
    {From, hangup, Freq} -> 
      From ! frequency_gen:deallocate(Freq);
    Msg -> 
      io:format("Unexpected client() message ~p~n", [Msg]),
      client()
  end.

%clear() ->
%  receive
%    Msg -> 
%      io:format("Received ~p after timeout~n", [Msg]), 
%      clear()
%  after 0 -> ok
%  end.

-spec add_client() -> {Freq::integer(), Pid::pid()} | {error, no_frequency}.
%% @doc Create a fresh client process and allocate a frequency if one is free, else kill process and return no_frequency message.
add_client() ->
  Pid = spawn(freqclient, client, []),
  Pid ! {self(), call},
  receive
    {ok, Freq} -> {Freq, Pid};
    {error, no_frequency} ->
       exit(Pid, no_frequency),
       {error, no_frequency} 
  end.

-spec delete_client({Freq::integer(), Pid::pid()}) -> ok.
%% @doc Make a frequency available for new clients, ending client loop.
delete_client({Freq, Pid}) ->
  Pid ! {self(), hangup, Freq},
  receive
    ok -> ok
  end.

-spec kill_client({Freq::integer(), Pid::pid()}) -> true.
%% @doc simulate process failing for the server to handle.
kill_client({_, Pid}) ->
  exit(Pid, battery_flat).

-spec random_test(N::integer()) -> ok.
%% Loop N times, randomly adding, deleting or killing clients.
random_test(N) ->
  frequency_gen:start(),
  simulate(N, [add_client(), add_client(), add_client(), add_client()]),
  frequency_gen:stop().

simulate(0, _) -> ok;
simulate(N, []) ->
  io:format("~p [] ~n", [N]),
  simulate(N-1, [add_client(), add_client(), add_client()]);
simulate(N, ClientList0) ->
  io:format("~p ~p ~n", [N, ClientList0]),
  case rand:uniform(4) of
    1 -> ClientList1 = simulate_add(ClientList0);
    2 -> ClientList1 = simulate_delete(ClientList0);
    3 -> ClientList1 = simulate_kill(ClientList0);
    4 -> ClientList1 = simulate_add(ClientList0)
  end,
  simulate(N-1, ClientList1).

simulate_add(ClientList) ->
  case add_client() of
    {error, no_frequency} -> ClientList;
    {Freq, Pid} -> [{Freq, Pid}| ClientList]
  end.

simulate_delete(ClientList) ->
  {Freq, Pid} = lists:nth(rand:uniform(length(ClientList)), ClientList),
  delete_client({Freq, Pid}),
  proplists:delete(Freq, ClientList).

simulate_kill(ClientList) ->
  {Freq, Pid} = lists:nth(rand:uniform(length(ClientList)), ClientList),
  kill_client({Freq, Pid}),
  proplists:delete(Freq, ClientList).

