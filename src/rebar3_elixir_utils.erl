-module(rebar3_elixir_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         modularize/2
         , modularize/1
         , to_list/1
        ]).

modularize(Prefix, Name) ->
  case modularize(Prefix) of
    "" ->
      modularize(Name);
    Prefix1 ->
      Prefix1 ++ "." ++ modularize(Name)
  end.

modularize(Name) when is_atom(Name) ->
  modularize(atom_to_list(Name));
modularize(Name) when is_binary(Name) ->
  modularize(binary_to_list(Name));
modularize(Name) when is_list(Name) ->
	string:join(lists:map(fun([H|R]) -> [string:to_upper(H)|R] end, string:tokens(Name, "/_.")), ".").

to_list(V) when is_atom(V) ->
  atom_to_list(V);
to_list(V) when is_list(V) ->
  V;
to_list(V) when is_binary(V); is_bitstring(V) ->
  binary_to_list(V).

-ifdef(TEST).
modularize_test() ->
  ?assertEqual("One", modularize("one")),
  ?assertEqual("One", modularize(one)),
  ?assertEqual("One", modularize(<<"one">>)),
  ?assertEqual("One.Two", modularize("One.Two")),
  ?assertEqual("One.Two", modularize("one.two")),
  ?assertEqual("One.Two", modularize("one_Two")),
  ?assertEqual("One.Two", modularize(one_Two)),
  ?assertEqual("One.Two", modularize(<<"one/Two">>)),
  ?assertEqual("One.Two.Three", modularize("one_two_three")),
  ?assertEqual("One.Two", modularize(one, two)),
  ?assertEqual("One.Two", modularize("one", two)),
  ?assertEqual("One.Two", modularize(<<"one">>, two)),
  ?assertEqual("One.Two", modularize(one, "two")),
  ?assertEqual("One.Two", modularize(one, <<"two">>)),
  ?assertEqual("One.Two", modularize("one", "two")),
  ?assertEqual("One.Two", modularize(<<"one">>, <<"two">>)),
  ?assertEqual("One.Two", modularize("one", <<"two">>)),
  ?assertEqual("One.Two", modularize(<<"one">>, "two")),
  ?assertEqual("Two", modularize("", "two")),
  ?assertEqual("Two", modularize('', "two")),
  ?assertEqual("ISModule", modularize("ISModule")),
  ?assertEqual("IS.A.MODULE", modularize("iS.a.MODULE")).
-endif.

