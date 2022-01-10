defmodule Xpeg.MixProject do
  use Mix.Project

  def project do
    [
      app: :xpeg,
      version: "0.5.1",
      description: "Native Elixir PEG (Parsing Expression Grammars) string matching library",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: [
        licenses: ["MIT"],
        links: %{"GitHub" => "https://github.com/zevv/xpeg"}
      ],
      docs: [
        extras: ["README.md"],
        logo: "xpeg.png"
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
      {:ex_doc, "~> 0.14"},
      {:exprof, "~> 0.2.0"},
      #{:poison, "~> 5.0"},
      #{:jason, "~> 1.2"},
    ]
  end
end
