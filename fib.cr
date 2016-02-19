def fib(n)
  if n >= 2 then
    return fib(n-1) + fib(n-2)
  end
  if n == 1 then
    return 1
  end
  return 0
end

puts(fib(10))
