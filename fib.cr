def fib(n)
  if n == 0 then
    return 0
  end
  if n == 1 then
    return 1
  else
    return fib(n-1) + fib(n-2)
  end
end

puts(fib(10))
