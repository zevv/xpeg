defmodule Xpeg.MixProject do
  use Mix.Project

  def project do
    [
      app: :xpeg,
      version: "0.1.0",
      description: "Native Elixir PEG (Parsing Expression Grammars) string matching library",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: [
        licenses: ["MIT"],
        links: %{ "GitHub" => "https://github.com/zevv/xpeg" },
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
