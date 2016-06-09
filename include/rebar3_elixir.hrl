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
      deps: deps
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
end").

