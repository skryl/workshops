# Functions
function fizz_buzz(n)
  for i=1:n
    if i % 15 == 0
      println("FizzBuzz")
    elseif i % 2 == 0
      println("Fizz")
    elseif i % 3 == 0
      println("Buzz")
    else 
      println(i)
    end
  end
end

function f1()
  function f2()
      println("f2")
  end
  f2()
end


# Ranges
typeof(1:5)
(1:5).start

for x=1:5
  println(x)
end

for x=1:2:10
  println(x)
end

names(1:2:10)
(1:2:10).step

# lambdas
 ((x) -> x + 5)(2)
 plus = x -> y -> x + y
 plus2 = plus(2)
 plus2(3)

# generic functions
fizz_buzz
+
methods(+)
@which fizz_buzz(100)

# overflow
2^64    # -> 0
int128(2)^64

# Types
Any
typeof(5)    # -> Int64
super(ans)   # -> Signed
super(ans)   # -> Integer
super(ans)   # -> Real
super(ans)   # -> Number
super(ans)   # -> Any

subtypes(Signed)

type Lion
  canroar
  taillength::Float64
  coatcolor
end

typeof(Lion) # -> DataType
typeof(ans)  # -> DataType

l = Lion(true, 5.0,"green")
l.coatcolor

type Cat
  canmeow
end

type Tiger <: Cat
  numberofstripes::Int
  favoritefood
end

super(Tiger)  # -> Cat

arr = Any[1,2,'h',"abc"]

type FancyType{T}
    data::T
    otherstuff::Vector{T}
end

FancyType(5, [1,2,3])
names(FancyType)

# methods for a type
methodswith(Dict, true)
methodswith(Array)

# Collections

# arrays / matrices
a = [1,2,3]
m = [1 2; 3 4]
typeof(m)

type SFtype2{T,I}
    data::Vector{T}
    function SFtype2(i:Int64)
        new([x for x = 1:i])
    end
end

# dictionaries
d = ["me" => 43, "hello" => 50]
for (k,v) = d
    println(k)
    println("\t$v")
end

# interpolation
"abc $(1 + 2 + 3)"

# docs
help(map)

# Macros

@show 2 + 2
@time sleep(0.1)
@elapsed sleep(0.1)

[x for x=1:3]
median([@elapsed sleep(0.1) for x = 1:10])

macro foo(x,y)
    quote
        2 + 2
    end
end

ast = :(2-3+4-5)
typeof(ast)   # -> Expr
names(ast)
ast.head
ast.args

q = quote
    println(x)
end
q.head
q.args

x = "Hello World"
eval(q)

repr(:(2 + 2))

macro bench(expr, times)
  println("Expression: $expr")
  println("Times: $times")
  quote
    median([@elapsed $expr for x=1:$times])
  end
end

@bench 2+2 10
@bench(2+2, 10)

methods(+, (Int64, Int64)) # -> lowered AST
finfer(+, (Int64, Int64))
disassemble(+, (Int64, Int64)) # -> llvm IR
disassemble(+, (Int64, Int64), true) # -> assembly

function fib(n)
  if n <= 1 1 else fib(n-1) + fib(n-2) end
end

expr = :[(x,y) for x=1:10, y=1:10]
