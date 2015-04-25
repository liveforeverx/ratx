defmodule Ratx.Mixfile do
  use Mix.Project
  @vsn "0.1.0"
  @github "https://github.com/liveforeverx/ratx"

  def project do
    [app: :ratx,
     version: @vsn,
     description: description,
     package: package]
  end

  defp description do
    """
    Rate limiter and overload protection for erlang and elixir applications.
    """
  end

  defp package do
    [files: ["src", "priv", "mix.exs", "rebar.config", "README*", "LICENSE*"],
     licenses: ["MIT"],
     links: %{"GitHub" => @github}]
   end
end
