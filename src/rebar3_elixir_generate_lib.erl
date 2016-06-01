-module(rebar3_elixir_generate_lib).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, generate_lib).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                         {module, ?MODULE},
                                                         {namespace, elixir},
                                                         {bare, true},
                                                         {deps, ?DEPS},
                                                         {example, "rebar3 elixir generate_lib"},
                                                         {short_desc, "Generate Elixir bindings."},
                                                         {desc, "Generate Elixir bindings."},
                                                         {opts, []}]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Apps = case rebar_state:current_app(State) of
           undefined ->
             rebar_state:project_apps(State);
           AppInfo ->
             [AppInfo]
         end,
  [begin
     Ebin = filename:join(rebar_app_info:out_dir(App), "ebin"),
     LibDir = filename:join(rebar_app_info:dir(App), "lib"),
     [generate_binding(Mod, Ebin, LibDir) ||
      Mod <- rebar_state:get(State, elixir_bindings, [])]
   end || App <- Apps],
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

generate_binding(Mod, Ebin, LibDir) ->
  LibFile = filename:join(LibDir, rebar3_elixir_utils:modularize(Mod) ++ ".ex"),
  rebar_api:info("Generate ~s", [LibFile]),
  Module = filename:join(Ebin, atom_to_list(Mod)),
  case code:load_abs(Module) of
    {error, Reason0} -> 
      rebar_api:error("Can't load module ~s: ~p", [Mod, Reason0]);
    {module, M} ->
      case erlang:apply(M, module_info, [exports]) of
        MI when is_list(MI) ->
          case filelib:ensure_dir(LibFile) of
            ok ->
              case file:open(LibFile, [write]) of
                {ok, IO} ->
                  io:format(IO, "# File: ~s.ex\n", [rebar3_elixir_utils:modularize(Mod)]),
                  io:format(IO, "# This file was generated from ~s.beam\n", [Mod]),
                  io:format(IO, "# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)\n", []),
                  io:format(IO, "# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!\n", []),
                  io:format(IO, "defmodule ~s do\n", [rebar3_elixir_utils:modularize(Mod)]),
                  lists:foreach(fun
                                  ({module_info, _}) -> ok;
                                  ({N, A}) ->
                                    Args = string:join(
                                             lists:map(fun(E) ->
                                                           "arg" ++ integer_to_list(E)
                                                       end, lists:seq(1,A)), ", "),
                                    io:format(IO, "  def unquote(:~p)(~s) do\n", [atom_to_list(N), Args]),
                                    io:format(IO, "    :erlang.apply(:~p, :~p, [~s])\n", [atom_to_list(M), atom_to_list(N), Args]),
                                    io:format(IO, "  end\n", [])
                                end, MI),
                  io:format(IO, "end\n", []),
                  ok = file:close(IO);
                {error, Reason1} ->
                  rebar_api:error("Can't create file ~s: ~p", [LibFile, Reason1])
              end;
            {error, Reason2} ->
              rebar_api:error("Can't create ~s: ~p", [LibDir, Reason2])
          end;
        _-> 
          rebar_api:error("Can't retrieve module info for ~s", [Mod])
      end
  end.
  
