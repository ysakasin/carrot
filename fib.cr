def fib(n)
  if n == 0 then
    return 0
  else
    if n == 1 then
      return 1
    else
      return fib(n-1) + fib(n-2)
    end
  end
end

puts(fib(10))

n = 0
if n then
  puts(n)
end

n = 1

if n then
  puts(n)
end

n = 0-1

if n then
  puts(n)
end
