%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% Tests can be run with <code>frequency:test()</code>
-module(frequency).
-export([ start/0
        , stop/0
        , allocate/0
        , deallocate/1
        , init/0
        , test/0
        ]).

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> true.
%% @doc spawn and register the frequency server.
start() -> 
  register(frequency, spawn(frequency, init, [])).

%% @doc this is only public to make spawn happy.
init() ->
  io:format("Starting...~n"),
  process_flag(trap_exit, true),
  try loop({[10,11,12,13,14,15], []}) of
    _ -> ok
  catch
    throw:{unknown_message, State} -> loop(State)
  end.

loop(State0) ->
  receive
    {request, From, allocate} ->
      {State1, Reply} = allocate(State0, From),
      From ! {reply, self(), Reply},
      loop(State1);
    {request, From, {deallocate, Freq}} ->
      try deallocate(State0, Freq) of
        State1 ->
          From ! {reply, self(), ok},
          loop(State1)
      catch
        throw:unallocated ->
          From ! {error, self(), unallocated},
          loop(State0)
      end;
    {request, From, stop} ->
      terminate(State0),
      From ! {reply, self(), stopped};
    {'EXIT', From, Reason} ->
      io:format("~p exited: ~p~n", [From, Reason]),
      State1 = exited(State0, From),
      loop(State1);
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
  Pid = whereis(frequency),  % modified provided example by checking received messages come from the server.
  frequency ! {request, self(), allocate},
  receive
    {reply, Pid, Reply} -> Reply
  after 1000 -> clear()
  end.

-spec deallocate(Freq::integer()) -> ok.
%% @doc deallocate a frequency.
deallocate(Freq) ->
  Pid = whereis(frequency),
  frequency ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Pid, ok} -> ok;
    {error, Pid, unallocated} -> ok
  after 1000 -> clear()
  end.

-spec stop() -> stopped.
%% @doc end listening loop.
stop() ->
  Pid = whereis(frequency),  
  frequency ! {request, self(), stop},
  receive
    {reply, Pid, Reply} -> 
      io:format("Terminating...~n"),
      Reply
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
exited({Free, Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} -> 
      {[Freq|Free], proplists:delete(Freq, Allocated)};
    false       -> 
      {Free, Allocated}
  end.

terminate({_, []}) -> ok;
terminate({Free, [{_, Pid}|Allocated]}) ->
  unlink(Pid),
  exit(Pid, normal),
  terminate({Free, Allocated}).

%% frequency:test().
%% rather use freqclient:random_test(1000).
test() ->
  start(), 
  allocate(),
  allocate(), 
  allocate(), 
  allocate(), 
  deallocate(20),  % Prints Unallocated frequency 20
  frequency ! wtf, % Prints Received unknown message wtf
  allocate(), 
  allocate(),
  allocate(), 
  deallocate(10), 
  allocate(),
  stop().

