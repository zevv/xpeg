defmodule Xpeg.MixProject do
  use Mix.Project

  def project do
    [
      app: :xpeg,
      version: "0.8.0",
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
      ],
      files: [
        "README.md",
        "mix*",
        "lib/codegen.ex",
        "lib/linker.ex",
        "lib/parser.ex",
        "lib/xpeg.ex"
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.14", only: [:dev], runtime: false},
      #{:exprof, "~> 0.2.0", only: [:dev, :test], runtime: false},
      #{:poison, "~> 5.0", only: [:dev, :test], runtime: false},
      #{:jason, "~> 1.2", only: [:dev, :test], runtime: false},
      #{:eflame, "~> 1.0", only: [:dev, :test], runtime: false},
    ]
  end
end
