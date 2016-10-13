-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  lists:foldl(fun provider_init/2, {ok, State}, [rebar3_elixir_generate_mix
                                                 , rebar3_elixir_generate_lib
                                                 , rebar3_elixir_generate_record]).

provider_init(Module, {ok, State}) ->
  Module:init(State).
