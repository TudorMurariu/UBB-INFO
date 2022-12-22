# Exilir in a nutshell ! <br>![Elixir photo](https://www.educative.io/cdn-cgi/image/f=auto,fit=contain,w=600/api/page/6426448461561856/image/download/5007090753470464)

# Summary
* What is Elixir?[^1]
* What is a functional language?[^2]
* Why Elixir?[^3]
* Hello world in Elixir.[^4]
* What I like about Elixir.[^5]
* What I dislike about Elixir.[^6]
* Why do I preffer Elixir over Lisp.[^7]

## [^1]: What is Elixir?

**Elixir is a dynamic, functional language for building scalable and maintainable applications.**  <br>
Elixir runs on the [Erlang VM](https://www.erlang.org/), so just like Java it is compatible with any operating system.
It is known for creating low-latency, distributed, and fault-tolerant systems. These capabilities and Elixir tooling allow developers to be productive in several domains, such as web development, embedded software, data pipelines, and multimedia processing, across a wide range of industries.

Here is a peek:
```elixir
# This function reads a number from a file 
# and returns it
def read() do
  {age, _} = IO.gets("Give: ") |> Integer.parse()
  age
end
```

## [^2]What is a functional language?
&nbsp; Functional programming languages are designed on the concept of **mathematical functions** that use **conditional expressions** and **recursion** to perform computation.
* For exemple, let's take this problem: <br>

&nbsp;Given an integer n (bigger than 0 and smaller than 100) find it's factorial. (the factorial of a number is each positive integer smaller than it multiplied by it, 4! = 1\*2\*3\*4 = 24) <br>
&nbsp; In an *Object Oriented Language* you'd use a loop, but in functional languages we use *recursion*. <br>
&nbsp;  Frist let's make the mathematical function and than we will write the code.

![factorial math](factorial.PNG) <br>

&nbsp; The elixir code for this code could be:
```elixir
def factorial(n) do
      cond do
        n > 0 -> n * factorial(n-1)
        true -> 1
    end
end
```

## [^3]Why Elixir?