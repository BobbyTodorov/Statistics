#zad1

x = USPersonalExpenditure[1:5];
y = USPersonalExpenditure[21:25];
shapiro.test(x);
shapiro.test(y);
t.test(x,y, paired = T);
#=> ednakvi sa zashtoto p-value e 9% , toest priemame hipotezata h0 : miux = miuy, ednakva e strukturata na potreblenie.

#zad2
x = morley[morley$Expt == 4,];
a = x$Speed;
shapiro.test(a);
qqnorm(a); # => normalno razpredeleni sa
t.test(a,conf.level = 0.97);

#zad3
x = c(83,35,42,48);
y = c(2/5,1/5,1/5,1/5);
chisq.test(x,p = y)
# p-value e 0.565 => 56% priemame hipotezata koqto e tvurdenieto na proizvoditelq.

#zad4
data = read.csv("E:/df1.txt");
l = lm(data$y ~ data$x1 + data$x2 + data$x3 + data$x4);
abline(l);
s = summary(l);
#ho : beta1 = -1.5
#h1 : beta1 != -1.5
beta1 = s$coefficients[4,1];
se = s$coefficients[4,2];
t = (beta1 + 1.5)/se;
p.val = 2*(pt(t,df = 25,lower.tail = F));
#=> golqmata stoinost na p.val oznachava che priemame hipotezata.

