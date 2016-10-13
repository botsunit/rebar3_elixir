-module(rebar3_elixir_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([modularize/1]).

modularize(Name) when is_atom(Name) ->
  modularize(atom_to_list(Name));
modularize(Name) when is_binary(Name) ->
  modularize(binary_to_list(Name));
modularize(Name) when is_list(Name) ->
	string:join(lists:map(fun([H|R]) -> [string:to_upper(H)|string:to_lower(R)] end, string:tokens(Name, "/_.")), ".").

-ifdef(TEST).
modularize_test() ->
  ?assertEqual("One", modularize("one")),
  ?assertEqual("One", modularize(one)),
  ?assertEqual("One.Two", modularize(one_two)),
  ?assertEqual("One.Two", modularize('One_two')),
  ?assertEqual("One.Two", modularize('One_Two')),
  ?assertEqual("One.Two", modularize(one_Two)),
  ?assertEqual("One.Two.Three", modularize(one_two_three)).
-endif.

