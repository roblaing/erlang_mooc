%% @doc Using gen_server conventions.
-module(echo_client).
-export([ start_link/0
        , terminal/1
        , init/1
        , count/0
        , echo/1
        , reset/1
        , stop/0
        , handle_call/3
        , handle_cast/2
        ]).

start_link() ->
  gen_server_light:start_link(echo_client, 0).

stop() -> 
  gen_server_light:stop(echo_client),
  stopped.

terminal(_) -> ok.

init(Init) -> Init.

%% Functional interfaces.
count()   -> gen_server_light:call(echo_client, count).
echo(Msg) -> gen_server_light:call(echo_client, {echo, Msg}).
reset(X)  -> gen_server_light:cast(echo_client, {reset, X}).

handle_call(count, _From, N) -> 
  {reply, N, N};
handle_call({echo, Msg}, _From, N) ->
  {reply, Msg, N+1}.

handle_cast({reset, X}, _N) ->
  {noreply, X};
handle_cast(stop, N) ->
  {noreply, N}.

