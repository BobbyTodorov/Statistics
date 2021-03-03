#x = survey[15:114,]

#1a
# women = x[x$Sex == 'Female',]
# women[which.max(women$Age),]
#1b
#i = order(x$Height, decreasing = T)
#x$Sex[i[1:4]]

#1v
# mean(x$Age[x$Sex == 'Male' & x$Pulse > 80], na.rm = T)

#1g
# t = table(x$Exer == 'Freq', x$Height > 175) ili t = table(x$Exer, x$Height > 175)
# sum(t[2, -1])  ili sum(t[1,-1])

#1d
#mean(x$Height, na.rm = T)
#median(x$Height, na.rm = T)
#sd(x$Height, na.rm = T)
#mode(x$Height)
#quantile(x$Height, na.rm = T)
#max(x$Height, na.rm = T)
#min(x$Height, na.rm = T)
#summary(x$Height)
#boxplot(x$Height ~ x$Exer, horizontal = T)

#1e
#plot(x$Age,x$Pulse, xlab = "Age", ylab = "Pulse")

#1j
#barplot(table(x$Exer,x$Smoke), beside = T, legend = T)



#zad2

#success = c(2:7) # broq na izteglenite beli
#X = dhyper(success, 7, 6, 8)
#m = 7
#n = 6
#k = 8
#p = m/(m+n)
#EX = k*p
#DX = k*p*(1-p)*(m + n - k)/(m + n -1)

#

#granicite

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

#grafikata


#zad3 

melon_third_quality = pnorm(20,25,6) # namirame veroqtnostta pupesh da e treto kachestvo
sum_first_and_second_quality = 1 - melon_third_quality # presmqtame veroqtnostta pupesh da e purvo ili vtoro kachestvo
melon_first_quality = sum_first_and_second_quality/2 # namirame pupesh da e purvo kachestvo ili vtoro (zashtoto sa ravni)
qnorm(melon_first_quality,25,6,lower.tail = FALSE) # namirame stoinostta na tegloto koqto opredelq razdelenieto na pupeshite na purvo i vtoro kachestvo


