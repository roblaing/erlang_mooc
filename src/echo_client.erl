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
        , handle_info/2
        ]).
% -define(SERVER, gen_server).
-define(SERVER, gen_server_light).
-behaviour(?SERVER).

start_link() ->
  ?SERVER:start_link({local, echo}, ?MODULE, 0, []).

stop() -> 
  ?SERVER:stop(echo),
  stopped.

-spec terminate(Reason::term(), State::term()) -> ok.
%% @doc Tried using shutdown instead of normal, and caused stop to break.
terminate(Reason, _) ->
  io:format("Terminated...~p~n", [Reason]).

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

handle_info(Info, State) ->
  io:format("Received unknown message ~p~n", [Info]),
  {noreply, State}.

