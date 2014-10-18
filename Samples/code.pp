~ Fibonacci Example
~ Reads a number 'n' and prints the 'n'th Fibonacci number

print "Input a number"
read n
a1 = 1
a2 = 0
while (n > 0)
  tmp = a1
  a1 = a1 + a2
  a2 = tmp
  n = n - 1
end
print a1
