#task 1
f = function(x)
{
  res = 0;
  
  for(i in 1:x)
  {
    res = res + 100*i^3/(9 + 4*(x^4));
  }
  
  return(res);
}

f1 = function(vec)
{
  res = rep(0,6);
  
  for(i in 1:length(vec))
  {
    res[i] = f(vec[i]);
  }
  
  return(res);
}

#task 2
#a
i = order(survey$Height, decreasing = T)
highest.students = survey[i[1:8],1]

#b
sum(survey$Sex == 'Female' & survey$Pulse > 75, na.rm = T)

#v
mens.height = survey$Height[survey$Sex == 'Male']
mens.height = mens.height[!is.na(mens.height)]
shapiro.test(mens.height) 
t.test(mens.height,conf.level = 0.97);

#g
women.pulse = survey$Pulse[survey$Sex == 'Female'];
women.pulse = women.pulse[!is.na(women.pulse)];

men.pulse = survey$Pulse[survey$Sex == 'Male'];
men.pulse = men.pulse[!is.na(men.pulse)];

shapiro.test(men.pulse);
shapiro.test(women.pulse);
t.test(women.pulse,men.pulse,alternative = "greater");

#d
l = lm(survey$Pulse ~ survey$Exer);
summary(l)

#task 3
rain = c(8.2, 6.4, 12.8, 7.6, 13.8, 16.1, 12.4, 12.8, 14.3, 13.5, 8.4, 7.3);
production = c(17.2,17.6,19.6,19.8,20.0,23.5,20.2,20.2,22.1,21.2,18.9,17.1);
l = lm(production ~ rain);
summary(l);
d = data.frame(rain = c(15));
predict.lm(l,d,interval = "confidence", conf.level = 0.97);
#equal:   15*s$coefficients[2,1] + s$coefficients[1,1]

#task 4
m = matrix(c(28, 42, 15, 23, 35, 17),nrow = 2, byrow = T);
chisq.test(m);

#zad5
data = read.csv("E:/Data1.txt", header = FALSE);
x = data$V1;
shapiro.test(x);
y = cut(x,breaks = c(0,2.5,5,7.5,10));
m = table(y);
chisq.test(m)
