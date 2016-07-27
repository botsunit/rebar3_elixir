-module(rebar3_elixir_generate_lib).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, generate_lib).
-define(DEPS, [{default, compile}]).

-include("../include/rebar3_elixir.hrl").

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
  ElixirVersion = rebar_state:get(State, elixir_version, ?DEFAULT_ELIXIR_VERSION),
  [begin
     Ebin = filename:join(rebar_app_info:out_dir(App), "ebin"),
     LibDir = filename:join(rebar_app_info:dir(App), "lib"),
     [generate_binding(Mod, Ebin, LibDir, ElixirVersion) ||
      Mod <- rebar_state:get(State, elixir_bindings, [])]
   end || App <- Apps],
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

generate_binding(ModuleName, Ebin, LibDir, ElixirVersion) when is_atom(ModuleName) ->
  generate_binding({ModuleName, []}, Ebin, LibDir, ElixirVersion);
generate_binding({ModuleName, Options}, Ebin, LibDir, ElixirVersion) ->
  Except = case lists:keyfind(except, 1, Options) of
             {except, E} when length(E) > 0 -> E;
             _ -> undefined
           end,
  Only = case lists:keyfind(only, 1, Options) of
           {only, O} when length(O) > 0 -> O;
           _ -> undefined
         end,
  LibFile = filename:join(LibDir, rebar3_elixir_utils:modularize(ModuleName) ++ ".ex"),
  Module = case code:load_abs(filename:join(Ebin, atom_to_list(ModuleName))) of
             {error, Reason0} -> 
               rebar_api:abort("Can't load module ~s: ~p", [ModuleName, Reason0]);
             {module, M} ->
               M
           end,
  IO = case filelib:ensure_dir(LibFile) of
         ok ->
           case file:open(LibFile, [write]) of
             {ok, IO0} ->
               rebar_api:info("Generate ~s", [LibFile]),
               IO0;
             {error, Reason1} ->
               rebar_api:abort("Can't create file ~s: ~p", [LibFile, Reason1])
           end;
         {error, Reason2} ->
           rebar_api:abort("Can't create ~s: ~p", [LibDir, Reason2])
       end,
  io:format(IO, "# File: ~s.ex\n", [rebar3_elixir_utils:modularize(ModuleName)]),
  io:format(IO, "# This file was generated from ~s.beam\n", [ModuleName]),
  io:format(IO, "# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)\n", []),
  io:format(IO, "# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!\n", []),
  io:format(IO, "defmodule ~s do\n", [rebar3_elixir_utils:modularize(ModuleName)]),
  HasCallbacks = write_callbacks(IO, Module),
  case version(ElixirVersion) >= "1.3" andalso HasCallbacks of
    true ->
      write_optional_callbacks(IO, Module);
    false ->
      false
  end,
  _ = write_functions(IO, Module, ModuleName, Except, Only),
  io:format(IO, "end\n", []),
  file:close(IO).

write_callbacks(IO, Module) ->
  case erlang:function_exported(Module, behaviour_info, 1) of
    true ->
      lists:foreach(fun({CallbackName, CallbackArity}) ->
                        io:format(IO, "  @callback ~s(~s) :: any\n", [CallbackName, any(CallbackArity)])
                    end, erlang:apply(Module, behaviour_info, [callbacks])),
      true;
    false ->
      false
  end.

write_optional_callbacks(IO, Module) ->
  case erlang:function_exported(Module, behaviour_info, 1) of
    true ->
      case erlang:apply(Module, behaviour_info, [optional_callbacks]) of
        [] -> 
          false;
        OptionalCallbacks ->
          io:format(IO, "  @optional_callbacks ~s\n", [to_ex(OptionalCallbacks)]),
          true
      end;
    false ->
      false
  end.

write_functions(IO, Module, ModuleName, Except, Only) ->
  case erlang:apply(Module, module_info, [exports]) of
    MI when is_list(MI) ->
      lists:foreach(fun
                      ({module_info, _}) -> ok;
                      ({behaviour_info, _}) -> ok;
                      ({N, A}) ->
                        case only(N, A, Only) andalso not except(N, A, Except) of
                          true ->
                            Args = string:join(
                                     lists:map(fun(E) ->
                                                   "arg" ++ integer_to_list(E)
                                               end, lists:seq(1,A)), ", "),
                            io:format(IO, "  def unquote(:~p)(~s) do\n", [atom_to_list(N), Args]),
                            io:format(IO, "    :erlang.apply(:~p, :~p, [~s])\n", [atom_to_list(Module), atom_to_list(N), Args]),
                            io:format(IO, "  end\n", []);
                          false ->
                            ok
                        end
                    end, MI);
    _-> 
      rebar_api:abort("Can't retrieve module info for ~s", [ModuleName])
  end.

only(_, _, undefined) ->
  true;
only(F, A, Only) ->
  lists:member({F, A}, Only) orelse lists:member(F, Only).

except(_, _, undefined) ->
  false;
except(F, A, Except) ->
  lists:member({F, A}, Except) orelse lists:member(F, Except).

any(N) ->
  string:join(lists:duplicate(N, "any"), ", ").

version(Version) ->
  re:replace(Version, "^[^0-9]*", "", [{return,list}]).

to_ex(List) when is_list(List) ->
  string:join([to_ex(E) || E <- List], ", ");
to_ex({A, B}) ->
  to_ex(A) ++ " " ++ to_ex(B);
to_ex(Atom) when is_atom(Atom) ->
  ?FMT("~s:", [Atom]);
to_ex(Num) when is_integer(Num);
                is_float(Num) ->
  ?FMT("~p", [Num]).

