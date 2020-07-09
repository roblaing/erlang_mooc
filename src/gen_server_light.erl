%% @doc Using gen_server conventions.
-module(gen_server_light).
-export([ start_link/4
        , stop/1
        , call/2
        , cast/2
        , init/2
        , loop/2
        ]).

-callback handle_call(Request::term(), {From::pid(), Ref::reference()}, State::term()) -> 
  Result :: {reply, Reply::term(), NewState::term()} | {stop, Reason::term(), NewState::term()}.
-callback handle_cast(Request::term(), State::term()) -> Result::{noreply, NewState::term()}.
-callback handle_info(Info::term(), State::term()) -> Result::{noreply, NewState::term()}.
-callback init(Args::term()) -> Result::{ok, State::term()}.
-callback terminate(Reason::term(), State::term()) -> none(). 

-spec start_link({local, RegName::atom()}, Module::module(), Arg::term(), Options::[term()]) -> {ok, pid()}.
%% @doc The original documentation says Args, leading to a common mistake of thinking there are surrounding list brackets.
start_link({local, RegName}, Module, Arg, _Options) ->
  Pid = spawn_link(?MODULE, init, [Module, Arg]),
  register(RegName, Pid),
  {ok, Pid}.

stop(Name) -> 
  Name ! stop,
  stopped.

init(Module, Arg) ->
  {ok, Init} = Module:init(Arg),
  loop(Module, Init).

loop(Module, State0) ->
  receive
    {call, Pid, Ref, Request} ->
      {reply, Reply, State1} = Module:handle_call(Request, {Pid, Ref}, State0),
      Pid ! {reply, Ref, Reply},
      loop(Module, State1);
    {cast, Request} -> 
      {noreply, State1} = Module:handle_cast(Request, State0),
      loop(Module, State1);
    stop -> Module:terminate(normal, State0);
    Unknown -> % includes {'EXIT', Pid, Reason} when process_flag(trap_exit, true)
      {noreply, State1} = Module:handle_info(Unknown, State0),
      loop(Module, State1)
  end.  

call(RegName, Request) ->
  Ref = monitor(process, whereis(RegName)),
  RegName ! {call, self(), Ref, Request},
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

%% @doc Asynchronous, does not need a reply from the server.
cast(RegName, Request) ->
  RegName ! {cast, Request},
  ok.

