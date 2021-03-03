arr = sample(10:20,3, replace = T)
x = arr[1]
y = arr[2]
z = arr[3]

Limits = function(x, y , z)
{
  p = x/(x + y)
  
  emp = rhyper(1000,x,y,z)
  
  EX = mean(emp)
  print(EX)
  
  DX = z*p*(1-p)*(x + y - z)/(x + y - 1)
  print(DX)
  
  return(c(min(emp),max(emp)))
}
  
EmpAndTheoDistribution = function(x, y, z)
{
  p = x/(x + y)
  
  if((z - y) < 0)
    success = 0
  else
    success = c((z - y):x)
  
  X = dhyper(success,x,y,z)
  print(X)
  print(EX)
  print(DX)
  
  emp = rhyper(1000,x,y,z)
  
  theorical.EX = z*p
  print(theorical.EX)
  
  Y = c(0,X*1000)
  hist(emp)
  lines(Y, col = 'red')
  
}

melon_third_quality = pnorm(20,25,6)
sum_first_and_second_quality = 1 - melon_third_quality 
melon_first_quality = sum_first_and_second_quality/2 
qnorm(melon_first_quality,25,6,lower.tail = FALSE) 

