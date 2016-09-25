# naive primality

function is_divisible(n, i)
  n % i == 0  
end

function is_prime(n::BigInt)
  for i=2:sqrt(n)
    if is_divisible(n, i)
      return false
    end
  end
  true
end

# quadratic solver

function derivative(f)
  h = 0.00001
  return function(x)
    f(x+h) - f(x) / h
  end
end

# fix fp errors

function derivative(f)
  return function(x)
    h = (x == 0 ? sqrt(eps(Float64)) : sqrt(eps(Float64)) * x)

    xph = x + h
    dx = xph - x
    f(xph) - f(x) / dx
  end
end

function quadratic(f)
  f1 = derivative(f)
  c = f(0.0)
  b = f1(0.0)
  a = f(1.0) - b - c

  return ((-b + sqrt(b^2 - 4a*c + 0im)) / 2a, 
          (-b - sqrt(b^2 - 4a*c + 0im)) / 2a)
end

# linear regression


