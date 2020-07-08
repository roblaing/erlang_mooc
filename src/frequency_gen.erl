%% @doc I've used <a href="http://erlang.org/doc/apps/edoc/chapter.html">Edoc</a> comment conventions.
%%
%% Tests can be run with <code>frequency_gen:test()</code>
-module(frequency_gen).
-export([ start/0
        , stop/0
        , init/1
        , allocate/0
        , deallocate/1
        , inject/2
        , report/0
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , test/0
        ]).
-vsn(1.0).
-define(SERVER, gen_server).
% -define(SERVER, gen_server_light).
-behaviour(?SERVER).

-type state() :: {Free::[Freq::integer()], Allocated::[{Freq::integer(), Pid::pid()}]}.

-spec start() -> ok.
%% @doc spawn and register the frequency servers.
start() ->
  ?SERVER:start_link({local, frequency1}, ?MODULE, {[10,11,12,13,14,15],[]}, []),
  ?SERVER:start_link({local, frequency2}, ?MODULE, {[20,21,22,23,24,25],[]}, []),
  ok.

-spec init(Init::state()) -> {ok, Init::state()}.
%% @doc Required by -behaviour(gen_server), must return an {ok, term()} tuple.
init(Init) ->
  process_flag(trap_exit, true),
  {ok, Init}.

-spec stop() -> stopped.
%% @doc call normal exit on clients and break listening loop
stop() ->
  io:format("Stopping frequency1... "),
  ?SERVER:stop(frequency1),
  io:format("Stopping frequency2... "),
  ?SERVER:stop(frequency2),
  stopped.

-spec terminate(Reason::atom(), State::state()) -> ok.
%% @doc Optional for -behaviour(gen_server), should return none(), but not sure how to do that.
terminate(Reason, {_, []}) -> 
  io:format("~p~n", [Reason]);
terminate(Reason, {Free, [{_, Pid}|Allocated]}) ->
  unlink(Pid), 
  exit(Pid, normal),
  terminate(Reason, {Free, Allocated}).

-spec allocate() -> {ok, Freq::integer()} | {error, no_frequency}.
%% @doc allocate a frequency, if possible.
allocate() ->
  Free1 = ?SERVER:call(frequency1, free),
  Free2 = ?SERVER:call(frequency2, free),
  if
    Free1 >= Free2 -> ?SERVER:call(frequency1, allocate);
    Free1 < Free2  -> ?SERVER:call(frequency2, allocate)
  end.

-spec deallocate(Freq::integer()) -> ok | {error, unallocated}.
%% @doc deallocate a frequency.
deallocate(Freq) ->
  if
    Freq >= 10, Freq =< 15 -> ?SERVER:call(frequency1, {deallocate, Freq});
    Freq >= 20, Freq =< 25 -> ?SERVER:call(frequency2, {deallocate, Freq});
    true -> ?SERVER:call(frequency1, {deallocate, Freq}) % for unallocated error test
  end.

-spec inject(Server :: atom(), [Freqs::integer()]) -> ok.
%% @doc append additionals Freqs to Free list.
inject(Server, Freqs) ->
  ?SERVER:cast(Server, {inject, Freqs}).

-spec report() -> ok.
%% Print current state of each server
report() ->
  io:format("Server 1 -- "),
  ?SERVER:call(frequency1, report),
  io:format("Server 2 -- "),
  ?SERVER:call(frequency2, report).

-spec handle_call(Request::term(), {Pid::pid(), Ref::reference()}, State0::state()) -> {reply, Reply::term(), State1::state()}.
%% @doc Note second argument From is a tuple {Pid, Ref}. This tripped me up rewriting the code.
handle_call(allocate, _, {[], Allocated}) -> 
  {reply, {error, no_frequency}, {[], Allocated}};
handle_call(allocate, {Pid, _}, {[Freq|Free], Allocated}) ->
  link(Pid),
  {reply, {ok, Freq}, {Free, [{Freq, Pid}|Allocated]}};

%% @doc Assumes deallocate request comes from the associated Pid. Could add another error message for when that's not the case.
handle_call({deallocate, Freq}, {Pid, _Ref}, {Free, Allocated}) ->
  case lists:member({Freq, Pid}, Allocated) of
    true  ->
      unlink(Pid),
      {reply, ok, {[Freq|Free], lists:delete({Freq, Pid}, Allocated)}};
    false ->
      io:format("Unallocated frequency ~p~n", [Freq]),
      {reply, {error, unallocated}, {Free, Allocated}}
  end;

handle_call(free, _From, {Free, Allocated}) ->
   {reply, length(Free), {Free, Allocated}};

handle_call(report, _From, {Free, Allocated}) ->
  io:format("Free: ~p Allocated ~p~n", [length(Free), length(Allocated)]),
  {reply, ok, {Free, Allocated}}.

handle_cast({inject, Freqs}, {Free, Allocated}) ->
  {noreply, {Free ++ Freqs, Allocated}}.

handle_info({'EXIT', Pid, Reason}, {Free, Allocated}) ->
  io:format("~p exited: ~p~n", [Pid, Reason]),
  case lists:keyfind(Pid, 2, Allocated) of
    {Freq, Pid} -> 
      {noreply, {[Freq|Free], proplists:delete(Freq, Allocated)}};
    false       -> 
      {noreply, {Free, Allocated}}
  end;

handle_info(Info, State) ->
  io:format("Received unknown message ~p~n", [Info]),
  {noreply, State}.

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

