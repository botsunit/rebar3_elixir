# rebar3_elixir

A plugins for [rebar3](http://www.rebar3.org/) to generate a compatible [mix.exs](http://elixir-lang.org/docs/stable/mix/Mix.html) file and Elixir bindings.

This plugin has two commands :

* `rebar3 elixir generate_mix` will generate the Mix file.
* `rebar3 elixir generate_lib` will generate Elixir bindings.

## Usage

__rebar3_elixir__ is available has a [hex](https://hex.pm/packages/rebar3_elixir) or [git](https://github.com/botsunit/rebar3_elixir) dependency :

Juste add :

```
{plugins, [rebar3_elixir]}.
```

or 

```
{plugins, [
  {rebar3_elixir, {git, "https://github.com/botsunit/rebar3_elixir.git"}}
]}.
```

in your `rebar.config`.

## Example

```rebar.config
```

`rebar3 elixir generate_mix` will generate :

```elixir
defmodule Test.Mixfile do
  use Mix.Project

  def project do
    [app: :test,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:lager], mod: {:test_app, []}]
  end

  defp deps do
    [ 
      {:lager, ~r/.*/, git: "https://github.com/basho/lager.git", branch: "master"},
      {:erlydtl, ~r/.*/, git: "https://github.com/erlydtl/erlydtl.git", branch: "master"},  
    ]
  end
end
```

`rebar3 elixir generate_lib` will generate an Elixir module in `lib` for each Erlang module. For example :

* From `src/example.erl`, `rebar3_elixir` will generate a module `lib/Example.ex`
* From `src/my_example.erl`, `rebar3_elixir` will generate a module `lib/My.Example.ex`
* From `src/other/simple_example.erl`, `rebar3_elixir` will generate a module `lib/Other.Simple.Example.ex`

You specify the modules for which you want a binding, by using the `elixir_bindings` option :

```makefile
{elixir_bindings, [example, simple_example]}. 
```

In this case, only `lib/Example.ex` and `lib/Other.Simple.Example.ex` will be generated.

You can also add a prefix by using `elixir_bindings_prefix` :

```makefile
{elixir_bindings_prefix, hello}.
```

So `rebar3_elixir` will generate `lib/Hello.Example.ex` and `lib/Hello.Other.Simple.Example.ex`

## Licence

Copyright (c) 2016, Bots Unit<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


