-module(frequency_SUITE).
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , test1/1
        ]).

all() -> [ test1
         ].

init_per_suite(Config) ->
  frequency:start(),
  Config.

end_per_suite(_Config) ->
  frequency:stop().

test1(_Config) ->
  ct:comment("~p", [frequency:allocate()]).

