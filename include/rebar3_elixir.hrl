-define(DEFAULT_ELIXIR_VERSION, "~> 1.2").
-define(FMT(F, V), lists:flatten(io_lib:format(F, V))).
-define(MIX_EXS, "defmodule ~s.Mixfile do
  use Mix.Project

  def project do
    [
      app: :~s,
      version: \"~s\",
      elixir: \"~s\",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
       applications: ~s,
       env: ~s~s
    ]
  end

  defp deps do
    [~s    
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run(\"compile\", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [~s
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [~s
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
      end).(command)
  end    
end").

