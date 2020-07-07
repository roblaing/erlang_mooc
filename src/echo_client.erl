%% @doc Toggle between my gen_server_light and OTP's gen_server by commenting and uncommenting the relevant define attribute.
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
% -define(SERVER, gen_server).
-define(SERVER, gen_server_light).
-behaviour(?SERVER).

start_link() ->
  {ok, Pid} = ?SERVER:start_link(?MODULE, 0, []),
  register(echo, Pid).

stop() -> 
  ?SERVER:stop(echo),
  stopped.

-spec terminate(Reason::term(), State::term()) -> ok.
terminate(normal, _) -> ok.

-spec init(Args::term()) -> Result::{ok, State::term()}.
init(Init) -> {ok, Init}.

%% Functional interfaces.
count()   -> ?SERVER:call(echo, count).
echo(Msg) -> ?SERVER:call(echo, {echo, Msg}).
reset(X)  -> ?SERVER:cast(echo, {reset, X}).

-spec handle_call(Request::term(), From::pid(), State::term()) -> Result::{reply, Reply::term(), NewState::term()}.
handle_call(count, _From, N) -> 
  {reply, N, N};
handle_call({echo, Msg}, _From, N) ->
  {reply, Msg, N+1}.

-spec handle_cast(Request::term(), State::term()) -> Result::{noreply, NewState::term()}.
handle_cast({reset, X}, _N) ->
  {noreply, X}.

