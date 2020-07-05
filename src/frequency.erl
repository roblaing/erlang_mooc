%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% Tests can be run with <code>frequency:test()</code>
-module(frequency).
-export([ start/0
        , stop/0
        , allocate/0
        , deallocate/1
        , inject/2
        , init/1
        , loop/1
        , test/0
        ]).
-vsn(1.4).

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> true.
%% @doc spawn and register the frequency servers.
start() -> 
  io:format("Starting...~n"),
  register(frequency1, spawn(frequency, init, [{[10,11,12,13,14,15],[]}])),
  register(frequency2, spawn(frequency, init, [{[20,21,22,23,24,25],[]}])).

-spec init(Init::state()) -> ok.
%% @doc this is only public to make spawn happy.
init(Init) ->
  process_flag(trap_exit, true),
  try loop(Init) of
    _ -> ok
  catch
    throw:{unknown_message, State} -> loop(State)
  end.

-spec loop(State :: state()) -> ok.
%% @doc needs to be public for hot module reloading to work.
loop(State0) ->
  % io:format("~p~n", [length(Free)]),
  receive
    {request, From, Ref, allocate} ->
      {State1, Reply} = allocate(State0, From),
      From ! {reply, Ref, Reply},
      frequency:loop(State1);
    {request, From, Ref, {deallocate, Freq}} ->
      try deallocate(State0, Freq) of
        State1 ->
          From ! {reply, Ref, ok},
          frequency:loop(State1)
      catch
        throw:unallocated ->
          From ! {reply, Ref, {error, unallocated}},
          frequency:loop(State0)
      end;
    {request, From, Ref, {inject, Freqs}} ->
      State1 = inject_(State0, Freqs),
      From ! {reply, Ref, ok},
      frequency:loop(State1);
    {request, From, Ref, free} ->
      {Free, _} = State0,
      From ! {reply, Ref, length(Free)},
      frequency:loop(State0);
    {request, From, Ref, stop} ->
      terminate(State0), % causes bugs
      From ! {reply, Ref, stopped};
    {'EXIT', From, Reason} ->
      io:format("~p exited: ~p~n", [From, Reason]),
      State1 = exited(State0, From),
      frequency:loop(State1);
    Unknown ->
      io:format("Received unknown message ~p~n", [Unknown]),
      throw({unknown_message, State0})
  end.

request(Server, Request) ->
  Ref = monitor(process, whereis(Server)),
  Server ! {request, self(), Ref, Request},
  receive
    {reply, Ref, Reply} -> 
      demonitor(Ref, [flush]), 
      Reply;
    {'DOWN', Ref, process, Pid, Info} ->
      io:format("Pid ~p Info ~p", [Pid, Info]),
      {error, server_down}
  end.

-spec allocate() -> {ok, Freq::integer()} | {error, no_frequency}.
%% @doc allocate a frequency, if possible.
allocate() ->
  Free1 = request(frequency1, free),
  Free2 = request(frequency2, free),
  if
    Free1 >= Free2 -> request(frequency1, allocate);
    Free1 < Free2  -> request(frequency2, allocate)
  end.

-spec deallocate(Freq::integer()) -> ok.
%% @doc deallocate a frequency.
deallocate(Freq) ->
  if
    Freq >= 10, Freq =< 15 -> request(frequency1, {deallocate, Freq});
    Freq >= 20, Freq =< 25 -> request(frequency2, {deallocate, Freq});
    true -> request(frequency1, {deallocate, Freq}) % for unallocated error test
  end.

-spec stop() -> stopped.
%% @doc end listening loop, enhanced to have handler kill client processes.
stop() ->
  io:format("Terminating...~n"),
  request(frequency1, stop),
  request(frequency2, stop).

-spec inject(Server :: atom(), [Freqs::integer()]) -> ok.
%% @doc append additionals Freqs to Free list.
inject(Server, Freqs) ->
  request(Server, {inject, Freqs}).

-spec allocate(State0::state(), Pid::pid()) -> {State1::state(), {ok, Freq::integer()} | {error, no_frequency}}.
%% @doc private auxiliary function for public allocate/0.
allocate({[], Allocated}, _) -> 
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

-spec deallocate(State0::state(), Freq::integer()) -> State1::state(). 
%% @doc private auxiliary function for public deallocate/1
%% throw exception when requested to deallocate an unallocated frequency
deallocate({Free, Allocated}, Freq) ->
  case proplists:lookup(Freq, Allocated) of
    {Freq, Pid} ->
      unlink(Pid),
      {[Freq|Free], proplists:delete(Freq, Allocated)};
    none        ->
      io:format("Unallocated frequency ~p~n", [Freq]),
      throw(unallocated)
  end.

-spec exited(State0::state(), Pid::pid()) -> State1::state().
%% @doc Handle {'EXIT', Pid, Reason} messages.
exited({Free, Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} -> 
      {[Freq|Free], proplists:delete(Freq, Allocated)};
    false       -> 
      {Free, Allocated}
  end.

terminate({_, []}) -> ok;
terminate({Free, [{_, Pid}|Allocated]}) ->
  unlink(Pid), % Just to be safe, though shouldn't be needed if exit Reason is normal
  exit(Pid, normal),
  terminate({Free, Allocated}).

inject_({Free, Allocated}, Freqs) ->
  {Free ++ Freqs, Allocated}.

%% frequency:test().
%% rather use freqclient:random_test(1000).
test() ->
  start(), 
  allocate(),
  allocate(), 
  allocate(), 
  allocate(), 
  deallocate(30),  % Prints Unallocated frequency 30
  inject(frequency1, [16,17]),
  frequency1 ! wtf, % Prints Received unknown message wtf
  allocate(), 
  allocate(),
  allocate(), 
  deallocate(10), 
  allocate(),
  stop().

