%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% Tests can be run with <code>frequency_gen:test()</code>
-module(frequency_gen).
-export([ start/0
        , stop/0
        , allocate/0
        , deallocate/1
        , inject/2
        , init/1
        , loop/1
        , test/0
        ]).
-vsn(1.0).

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> true.
%% @doc spawn and register the frequency servers.
start() -> 
  register(frequency1, spawn(frequency_gen, init, [{[10,11,12,13,14,15],[]}])),
  register(frequency2, spawn(frequency_gen, init, [{[20,21,22,23,24,25],[]}])).

-spec init(Init::state()) -> ok.
%% @doc this is only public to make spawn happy.
init(Init) ->
  io:format("Starting...~n"),
  loop(Init).

-spec loop(State :: state()) -> ok.
%% @doc needs to be public for hot module reloading to work.
loop(State0) ->
  % timer:sleep(1000), % simulate busy server
  receive
    {request, From, Ref, stop} ->
      terminate(shutdown, State0),
      From ! {reply, Ref, stopped};
    {request, From, Ref, Request} ->
      {reply, Reply, State1} = handle_call(Request, From, State0),
      From ! {reply, Ref, Reply},
      loop(State1);
    {'EXIT', From, Reason} ->  % handle_info/2 in gen_server
      io:format("~p exited: ~p~n", [From, Reason]),
      State1 = exited(State0, From),
      loop(State1);
    Unknown ->
      io:format("Received unknown message ~p~n", [Unknown]),
      loop(State0)
  end.

-spec call(RegName::atom(), Request :: term()) -> Response :: term().
%% @doc I changed the name from request to call after discovering that's the OTP idiom.
call(RegName, Request) ->
  Ref = monitor(process, whereis(RegName)),
  RegName ! {request, self(), Ref, Request},
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

-spec stop() -> stopped.
%% @doc call normal exit on clients and break listening loop
stop() ->
  call(frequency1, stop),
  call(frequency2, stop).

-spec allocate() -> {ok, Freq::integer()} | {error, no_frequency}.
%% @doc allocate a frequency, if possible.
allocate() ->
  Free1 = call(frequency1, free),
  Free2 = call(frequency2, free),
  if
    Free1 >= Free2 -> call(frequency1, allocate);
    Free1 < Free2  -> call(frequency2, allocate)
  end.

-spec deallocate(Freq::integer()) -> ok | {error, unallocated}.
%% @doc deallocate a frequency.
deallocate(Freq) ->
  if
    Freq >= 10, Freq =< 15 -> call(frequency1, {deallocate, Freq});
    Freq >= 20, Freq =< 25 -> call(frequency2, {deallocate, Freq});
    true -> call(frequency1, {deallocate, Freq}) % for unallocated error test
  end.

-spec inject(Server :: atom(), [Freqs::integer()]) -> ok.
%% @doc append additionals Freqs to Free list.
inject(Server, Freqs) ->
  call(Server, {inject, Freqs}).

-spec exited(State0::state(), Pid::pid()) -> State1::state().
%% @doc Handle {'EXIT', Pid, Reason} messages.
exited({Free, Allocated}, Pid) ->
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} -> 
      {[Freq|Free], proplists:delete(Freq, Allocated)};
    false       -> 
      {Free, Allocated}
  end.

% https://erlang.org/doc/man/gen_server.html#Module:terminate-2 Module:terminate(Reason, State)
terminate(shutdown, {_, []}) -> 
  io:format("Terminated...~n"),
  ok;
terminate(shutdown, {Free, [{_, Pid}|Allocated]}) ->
  unlink(Pid), % Just to be safe, though shouldn't be needed if exit Reason is normal
  exit(Pid, normal),
  terminate(shutdown, {Free, Allocated}).

-spec handle_call(Request::term(), From::pid(), State0::state()) -> {reply, Reply::term(), State1::state()}.
%% @doc {reply,Reply,NewState}
handle_call(allocate, _, {[], Allocated}) -> 
  {reply, {error, no_frequency}, {[], Allocated}};
handle_call(allocate, From, {[Freq|Free], Allocated}) ->
  link(From),
  {reply, {ok, Freq}, {Free, [{Freq, From}|Allocated]}};

%% @doc Assumes deallocate request comes from associated Pid. Could add another error message for when that's not the case.
handle_call({deallocate, Freq}, From, {Free, Allocated}) ->
  case lists:member({Freq, From}, Allocated) of
    true ->
      unlink(From),
      {reply, ok, {[Freq|Free], proplists:delete(Freq, Allocated)}};
    false ->
      io:format("Unallocated frequency ~p~n", [Freq]),
      {reply, {error, unallocated}, {Free, Allocated}}
  end;

handle_call({inject, Freqs}, _From, {Free, Allocated}) ->
  {reply, ok, {Free ++ Freqs, Allocated}};

handle_call(free, _From, {Free, Allocated}) ->
   {reply, length(Free), {Free, Allocated}}.

%% frequency_gen:test().
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

