%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% EUnit tests can be run with <code>frequency:test()</code>
-module(frequency).
-export([ start/0
        , stop/0
        , allocate/0
        , deallocate/1
        , init/0
        ]).

-include_lib("eunit/include/eunit.hrl").

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> true.
%% @doc spawn and register the server.
start() -> 
  register(frequency, spawn(frequency, init, [])).

%% @doc this is only public to make spawn happy.
init() ->
  io:format("Starting...~n"),
  loop({[10,11,12,13,14,15], []}).

loop(State0) ->
  % timer:sleep(1500), % simulate busy server
  receive
    {request, From, allocate} ->
      {State1, Reply} = allocate(State0, From),
      From ! {reply, self(), Reply},
      loop(State1);
    {request, From, {deallocate, Freq}} ->
      State1 = deallocate(State0, Freq),
      From ! {reply, self(), ok},
      loop(State1);
    {request, From, stop} ->
      From ! {reply, self(), stopped}
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
    {reply, Pid, Reply} -> Reply
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
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

-spec deallocate(State0::state(), Freq::integer()) -> {State1::state()}. 
%% @doc private auxiliary function for public deallocate/1
deallocate({Free, Allocated}, Freq) ->
  case proplists:is_defined(Freq, Allocated) of
    true  -> {[Freq|Free], proplists:delete(Freq, Allocated)};
    false -> {Free, Allocated}
  end.

%% frequency:test().
%% fails unless timer:sleep(1500) is commented out of loop(State).
frequency1_test_() ->
  [ ?_assertEqual(start(), true)
  , ?_assertEqual(allocate(), {ok, 10})
  , ?_assertEqual(allocate(), {ok, 11})
  , ?_assertEqual(allocate(), {ok, 12})
  , ?_assertEqual(allocate(), {ok, 13})
  , ?_assertEqual(allocate(), {ok, 14})
  , ?_assertEqual(allocate(), {ok, 15})
  , ?_assertEqual(allocate(), {error, no_frequency})
  , ?_assertEqual(deallocate(10), ok)
  , ?_assertEqual(allocate(), {ok, 10})
  , ?_assertEqual(stop(), stopped)
  ].

