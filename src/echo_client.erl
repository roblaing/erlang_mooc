%% @doc Using gen_server conventions.
-module(echo_client).
-export([ start_link/0
        , terminate/2
        , init/1
        , count/0
        , echo/1
        , reset/1
        , stop/0
        , handle_call/3
        , handle_cast/2
        ]).
-behaviour(gen_server_light).

start_link() ->
  gen_server_light:start_link(echo, ?MODULE, 0, []).

stop() -> 
  gen_server_light:stop(echo),
  stopped.

-spec terminate(Reason::term(), State::term()) -> ok.
terminate(normal, _) -> ok.

-spec init(Args::term()) -> Result::{ok, State::term()}.
init(Init) -> {ok, Init}.

%% Functional interfaces.
count()   -> gen_server_light:call(echo, count).
echo(Msg) -> gen_server_light:call(echo, {echo, Msg}).
reset(X)  -> gen_server_light:cast(echo, {reset, X}).

-spec handle_call(Request::term(), From::pid(), State::term()) -> Result::{reply, Reply::term(), NewState::term()}.
handle_call(count, _From, N) -> 
  {reply, N, N};
handle_call({echo, Msg}, _From, N) ->
  {reply, Msg, N+1}.

-spec handle_cast(Request::term(), State::term()) -> Result::{noreply, NewState::term()}.
handle_cast({reset, X}, _N) ->
  {noreply, X}.

