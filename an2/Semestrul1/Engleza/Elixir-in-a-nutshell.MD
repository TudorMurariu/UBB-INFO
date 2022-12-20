# Exilir in a nutshell ![Elixir photo](https://www.educative.io/cdn-cgi/image/f=auto,fit=contain,w=600/api/page/6426448461561856/image/download/5007090753470464)

# Summary
* What is Elixir?[^1]
* Why Elixir?[^2]
* Hello world in elixir.[^3]
* Why do I preffer Elixir over Lisp.[^4]

[^1]:## What is Elixir?

**Elixir is a dynamic, functional language for building scalable and maintainable applications.**  <br>
Elixir runs on the [Erlang VM](https://www.erlang.org/), so just like Java it is compatible with any operating system.
It is known for creating low-latency, distributed, and fault-tolerant systems. These capabilities and Elixir tooling allow developers to be productive in several domains, such as web development, embedded software, data pipelines, and multimedia processing, across a wide range of industries.

Here is a peek:
```elixir
def read() do
    {age, _} = IO.gets("Give: ") |> Integer.parse()
    age
  end
```

