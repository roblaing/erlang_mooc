-module(calc).
-export([ start/1
        , init/1
        , stop/0
        , eval/1
        , handle/2
        , terminate/1
        ]).

start(Env) ->
  server:start(calc, Env).

init(Env) -> 
  io:format("Starting...~n"),
  Env.

stop() ->
  server:stop(calc).

eval(Expr) ->
  server:request(calc, {eval, Expr}).

handle({eval, Expr}, Env) ->
  {expr:eval(Env, Expr), Env}.

terminate(_Env) ->
  io:format("Terminating...~n").

