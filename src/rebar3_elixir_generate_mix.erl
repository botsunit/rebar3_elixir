-module(rebar3_elixir_generate_mix).
-behaviour(provider).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, generate_mix).
-define(DEPS, [{default, compile}]).

-include("../include/rebar3_elixir.hrl").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                         {module, ?MODULE},
                                                         {namespace, elixir},
                                                         {bare, true},
                                                         {deps, ?DEPS},
                                                         {example, "rebar3 elixir generate_mix"},
                                                         {short_desc, "Generate mix.exs."},
                                                         {desc, "Generate mix.exs."},
                                                         {opts, []}]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Apps = case rebar_state:current_app(State) of
           undefined ->
             rebar_state:project_apps(State);
           AppInfo ->
             [AppInfo]
         end,
  ElixirVersion = rebar_state:get(State, elixir_version, ?DEFAULT_ELIXIR_VERSION),
  [begin
     AppName = rebar_app_info:name(App),
     AppVsn = rebar_app_info:original_vsn(App),
     AppApplications = rebar_app_info:applications(App) -- [kernel, stdlib],
     AppDetails = rebar_app_info:app_details(App),
     AppDeps = rebar_state:get(State, deps, []),
     AppProfiles = rebar_state:get(State, profiles, []),
     AppDir = rebar_app_info:dir(App),
     MixFile = filename:join(AppDir, "mix.exs"), 
     DefaultDeps = lists:foldl(fun
                                 ({Profile, Data}, DepsAcc) when Profile == default;
                                                                 Profile == prod;
                                                                 Profile == dev ->
                                   case lists:keyfind(deps, 1, Data) of
                                     {deps, Deps} ->
                                       DepsAcc ++ get_deps(Deps, Profile);
                                     _ ->
                                       DepsAcc
                                   end;
                                 (_, DepsAcc) ->
                                   DepsAcc
                               end, get_deps(AppDeps), AppProfiles),
     FormatedDeps = string:join(DefaultDeps, ","),
     AppEnv = case lists:keyfind(env, 1, AppDetails) of
                {env, Env} ->
                  Env;
                _ ->
                  []
              end,
     AppMod = case lists:keyfind(mod, 1, AppDetails) of
                {mod, Mod} ->
                  ?FMT(",~n       mod: ~s", [to_ex(Mod)]);
                _ ->
                  ""
              end,
     PreCompileHooks = string:join(get_hooks(App, pre_hooks, compile), ","),
     PostCompileHooks = string:join(get_hooks(App, post_hooks, compile), ","),
     rebar_api:info("Create ~s", [MixFile]),
     file:write_file(MixFile,
                     ?FMT(?MIX_EXS, [rebar3_elixir_utils:modularize(AppName),
                                     AppName,
                                     AppVsn,
                                     ElixirVersion,
                                     to_ex(AppApplications),
                                     to_ex(AppEnv),
                                     AppMod,
                                     FormatedDeps,
                                     PreCompileHooks,
                                     PostCompileHooks]))
   end || App <- Apps],
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

get_deps(Deps) ->
  get_deps(Deps, [], "").
get_deps(Deps, Profile) ->
  get_deps(Deps, [], ?FMT(", only: :~s", [Profile])).

get_deps([], Acc, _) ->
  lists:reverse(Acc);
get_deps([Name|Deps], Acc, Only) when is_atom(Name) ->
  get_deps(Deps, [?FMT("~n      {:~s, \"> 0\"~s}", [Name, Only])|Acc], Only);
get_deps([{Name, Version}|Deps], Acc, Only) when is_atom(Name), is_list(Version) ->
  get_deps(Deps, [?FMT("~n      {:~s, \"~s\"~s}", [Name, Version, Only])|Acc], Only);
get_deps([{Name, {pkg, HexName}}|Deps], Acc, Only) when is_atom(Name) ->
  get_deps(Deps, [?FMT("~n      {:~s, \"> 0\"~s, hex: :~s}", [Name, Only, HexName])|Acc], Only);
get_deps([{Name, Version, {pkg, HexName}}|Deps], Acc, Only) when is_atom(Name), is_list(Version) ->
  get_deps(Deps, [?FMT("~n      {:~s, \"~s\"~s, hex: :~s}", [Name, Version, Only, HexName])|Acc], Only);
get_deps([{Name, {git, URL}}|Deps], Acc, Only) when is_atom(Name), is_list(URL) ->
  get_deps(Deps, [?FMT("~n      {:~s, git: \"~s\"~s}", [Name, URL, Only])|Acc], Only);
get_deps([{Name, {git, URL, {Type, RTB}}}|Deps], Acc, Only) when is_atom(Name), is_list(URL), is_atom(Type), is_list(RTB) ->
  get_deps(Deps, [?FMT("~n      {:~s, git: \"~s\", ~s: \"~s\"~s}", [Name, URL, Type, RTB, Only])|Acc], Only);
get_deps([{Name, Reg, {git, URL}}|Deps], Acc, Only) when is_atom(Name), is_list(Reg), is_list(URL) ->
  get_deps(Deps, [?FMT("~n      {:~s, ~~r/~s/, git: \"~s\"~s}", [Name, Reg, URL, Only])|Acc], Only);
get_deps([{Name, Reg, {git, URL, {Type, RTB}}}|Deps], Acc, Only) when is_atom(Name), is_list(Reg), is_list(URL), is_atom(Type), is_list(RTB) ->
  get_deps(Deps, [?FMT("~n      {:~s, ~~r/~s/, git: \"~s\", ~s: \"~s\"~s}", [Name, Reg, URL, Type, RTB, Only])|Acc], Only).

get_hooks(App, Type, Section) ->
  Opts = rebar_app_info:opts(App),
  Hooks = rebar_opts:get(Opts, Type, []),
  lists:reverse(
    lists:foldl(
      fun
        ({Regex, S, Command}, Acc) when S == Section ->
          [?FMT("~n      {\"~s\", \"~s\"}", [Regex, Command])|Acc];
        ({S, Command}, Acc) when S == Section ->
          [?FMT("~n      \"~s\"", [Command])|Acc];
        (_, Acc) ->
          Acc
      end, [], Hooks)).
  
-ifdef(TEST).
deps_test() ->
  ?assertEqual(
     [
      "\n      {:one, \"> 0\"}",
      "\n      {:two, \"~> 2.0.0\"}",
      "\n      {:three, \"> 0\", hex: :real_three}",
      "\n      {:four, \"~> 4.0.0\", hex: :real_four}",
      "\n      {:five, git: \"http://five.git\"}",
      "\n      {:six, git: \"http://six.git\", branche: \"master\"}",
      "\n      {:seven, ~r/7.*/, git: \"http://seven.git\"}",
      "\n      {:eight, ~r/8.*/, git: \"http://eight.git\", tag: \"8.0.0\"}"
     ],
     get_deps([
               one,
               {two, "~> 2.0.0"},
               {three, {pkg, real_three}},
               {four, "~> 4.0.0", {pkg, real_four}},
               {five, {git, "http://five.git"}},
               {six, {git, "http://six.git", {branche, "master"}}},
               {seven, "7.*", {git, "http://seven.git"}},
               {eight, "8.*", {git, "http://eight.git", {tag, "8.0.0"}}}
              ])),
  ?assertEqual(
     [
      "\n      {:one, \"> 0\", only: :test}",
      "\n      {:two, \"~> 2.0.0\", only: :test}",
      "\n      {:three, \"> 0\", only: :test, hex: :real_three}",
      "\n      {:four, \"~> 4.0.0\", only: :test, hex: :real_four}",
      "\n      {:five, git: \"http://five.git\", only: :test}",
      "\n      {:six, git: \"http://six.git\", branche: \"master\", only: :test}",
      "\n      {:seven, ~r/7.*/, git: \"http://seven.git\", only: :test}",
      "\n      {:eight, ~r/8.*/, git: \"http://eight.git\", tag: \"8.0.0\", only: :test}"
     ],
     get_deps([
               one,
               {two, "~> 2.0.0"},
               {three, {pkg, real_three}},
               {four, "~> 4.0.0", {pkg, real_four}},
               {five, {git, "http://five.git"}},
               {six, {git, "http://six.git", {branche, "master"}}},
               {seven, "7.*", {git, "http://seven.git"}},
               {eight, "8.*", {git, "http://eight.git", {tag, "8.0.0"}}}
              ], test)).
-endif.

to_ex([]) ->
  "[]";
to_ex(List) when is_list(List) ->
  case io_lib:printable_list(List) of
    true ->
      ?FMT("'~s'", [List]);
    false ->
      "[" ++ string:join([to_ex(E) || E <- List], ", ") ++ "]"
  end;
to_ex(Tuple) when is_tuple(Tuple) ->
  "{" ++ string:join([to_ex(element(I, Tuple)) || I <- lists:seq(1, tuple_size(Tuple))], ", ") ++ "}";
to_ex(Atom) when is_atom(Atom) ->
  ?FMT(":~s", [Atom]);
to_ex(Binary) when is_binary(Binary) ->
  ?FMT("\"~s\"", [Binary]);
to_ex(Num) when is_integer(Num);
                is_float(Num) ->
  ?FMT("~p", [Num]).
  
-ifdef(TEST).
to_ex_test() ->
  ?assertEqual(":hello", to_ex(hello)),
  ?assertEqual("\"hello\"", to_ex(<<"hello">>)),
  ?assertEqual("123", to_ex(123)),
  ?assertEqual("123.45", to_ex(123.45)),
  ?assertEqual("'hello world'", to_ex("hello world")),
  ?assertEqual("[]", to_ex([])),
  ?assertEqual("[]", to_ex("")),
  ?assertEqual("[:a, :b, :c]", to_ex([a, b, c])),
  ?assertEqual("[\"hello\", \"world\"]", to_ex([<<"hello">>, <<"world">>])),
  ?assertEqual("[123, 456, 789]", to_ex([123, 456, 789])),
  ?assertEqual("[123.98, 456.76, 789.54]", to_ex([123.98, 456.76, 789.54])),
  ?assertEqual("{:a, :b, :c}", to_ex({a, b, c})),
  ?assertEqual("{\"hello\", \"world\"}", to_ex({<<"hello">>, <<"world">>})),
  ?assertEqual("{123, 456, 789}", to_ex({123, 456, 789})),
  ?assertEqual("{123.98, 456.76, 789.54}", to_ex({123.98, 456.76, 789.54})),
  ?assertEqual("[123, 456.78, \"hello\", :atom, [1, :two, \"three\", {1, :two, \"three\", [:a, :b]}]]", 
               to_ex([123, 456.78, <<"hello">>, atom, [1, two, <<"three">>, {1, two, <<"three">>, [a, b]}]])).
-endif.

