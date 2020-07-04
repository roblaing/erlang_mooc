%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% Tests can be run with <code>frequency:test()</code>
-module(frequency).
-export([ start/0
        , stop/0
        , allocate/0
        , deallocate/1
        , inject/1
        , init/1
        , loop/1
        , test/0
        ]).
-vsn(1.3).

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> true.
%% @doc spawn and register the frequency server.
start() -> 
  io:format("Starting...~n"),
  register(frequency1, spawn(frequency, init, [[10,11,12,13,14,15]])),
  register(frequency2, spawn(frequency, init, [[20,21,22,23,24,25]])).

-spec init(Freqs::[integer()]) -> ok.
%% @doc this is only public to make spawn happy.
init(Freqs) ->
  process_flag(trap_exit, true),
  try loop({Freqs, []}) of
    _ -> ok
  catch
    throw:{unknown_message, State1} -> loop(State1)
  end.

-spec loop(State :: state()) -> ok.
%% @doc needs to be public for hot module reloading to work.
loop(State0) ->
  % io:format("~p~n", [length(Free)]),
  receive
    {request, From, allocate} ->
      {State1, Reply} = allocate(State0, From),
      From ! {reply, self(), Reply},
      frequency:loop(State1);
    {request, From, {deallocate, Freq}} ->
      try deallocate(State0, Freq) of
        State1 ->
          From ! {reply, self(), ok},
          frequency:loop(State1)
      catch
        throw:unallocated ->
          From ! {error, self(), unallocated},
          frequency:loop(State0)
      end;
    {request, From, {inject, Freqs}} ->
      State1 = inject(State0, Freqs),
      From ! {reply, self(), ok},
      frequency:loop(State1);
    {request, From, free} ->
      {Free, _} = State0,
      From ! {reply, self(), length(Free)},
      frequency:loop(State0);
    {request, From, stop} ->
      terminate(State0),
      From ! {reply, self(), stopped};
    {'EXIT', From, Reason} ->
      io:format("~p exited: ~p~n", [From, Reason]),
      State1 = exited(State0, From),
      frequency:loop(State1);
    Unknown ->
      io:format("Received unknown message ~p~n", [Unknown]),
      throw({unknown_message, State0})
  end.

clear() ->
  receive
    Msg -> 
      io:format("Received ~p after timeout~n", [Msg]), 
      clear()
  after 0 -> ok
  end.

-spec allocate() -> {ok, Freq::integer()} | {error, no_frequency}.
%% @doc allocate a frequency, if possible.
allocate() ->
  Free1 = get_free(frequency1),
  Free2 = get_free(frequency2),
  if
    Free1 >= Free2 -> allocate_(frequency1);
    Free1 < Free2  -> allocate_(frequency2)
  end.

allocate_(Server) ->
  Pid = whereis(Server),
  Pid ! {request, self(), allocate},
  receive
    {reply, Pid, {ok, Freq}} -> {ok, Freq};
    {reply, Pid, {error, no_frequency}} -> {error, no_frequency}
  after 1000 -> clear()
  end.

get_free(Server) ->
  Pid = whereis(Server),
  Pid ! {request, self(), free},
  receive
    {reply, Pid, Free} -> Free
  after 1000 -> 0
  end.

-spec deallocate(Freq::integer()) -> ok.
%% @doc deallocate a frequency.
deallocate(Freq) ->
  if
    Freq >= 10, Freq =< 15 -> deallocate_(frequency1, Freq);
    Freq >= 20, Freq =< 25 -> deallocate_(frequency2, Freq);
    true -> deallocate_(frequency1, Freq) % for unallocated error test
  end.

deallocate_(Server, Freq) ->
  Pid = whereis(Server),
  Pid ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Pid, ok} -> ok;
    {error, Pid, unallocated} -> ok % do something more specific in real life.
  after 1000 -> clear()
  end.

-spec stop() -> stopped.
%% @doc end listening loop, enhanced to have handler kill client processes.
stop() ->
  io:format("Terminating...~n"),
  stop_(frequency1),
  stop_(frequency2).

stop_(Server) ->
  Pid = whereis(Server),  
  Pid ! {request, self(), stop},
  receive
    {reply, Pid, stopped} -> stopped
  after 1000 -> clear()
  end.

-spec inject([Freqs::integer()]) -> ok.
%% @doc append additionals Freqs to Free list.
inject(Freqs) ->
  Pid = whereis(frequency1),  
  frequency1 ! {request, self(), {inject, Freqs}},
  receive
    {reply, Pid, ok} -> ok
  after 1000 -> clear()
  end.

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
  unlink(Pid), % still needed if exit Reason is normal?
  exit(Pid, normal),
  terminate({Free, Allocated}).

inject({Free, Allocated}, Freqs) ->
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
  inject([16,17]),
  frequency1 ! wtf, % Prints Received unknown message wtf
  allocate(), 
  allocate(),
  allocate(), 
  deallocate(10), 
  allocate(),
  stop().

