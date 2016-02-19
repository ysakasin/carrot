def fib(n)
  if n >= 2 then
    return fib(n-1) + fib(n-2)
  elsif n == 1 then
    return 1
  else
    return 0
  end
end

puts(fib(10))
