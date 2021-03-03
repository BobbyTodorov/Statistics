#task 1
#a
sum(Aids$age < 20);

#b
i = order(Aids2$diag)
Aids2[i[1:5],]

#1v
men = Aids2[Aids2$sex == 'M',];
men.blood = sum(men$T.categ == 'blood');
p = men.blood/length(men[,1]); # 0.02069717

#1g
barplot(prop.table(table(status,state), 2), legend = T);

#task 2
prop.test(c(p[2,1],p[2,2]), c(sum(Aids2$sex == 'F'), sum(Aids2$sex == 'M')), alternative = "less");

#task 3
x = Aids2[Aids2$status == 'D',];
y = (x$death - x$diag)/365 + x$age
wilcox.test(y,mu = 38);

#task 4
x = rchisq(100,10);
hist(x, probability = T);
curve(dchisq(x,10), add = T);

#task 5
men.cats = cats[cats$Sex == 'M',];
x= men.cats$Bwt;
y = men.cats$Hwt;
l = lm(y ~ x);
plot(x,y);
abline(l);
s = summary(l);

beta1 = s$coefficients[2,1]
se = s$coefficients[2,2]
t = (beta1 - 5)/se
p = 2*pt(t,df = 95)

d = data.frame(x = 2.6);
predict.lm(l,d,interval = "confidence",conf.level = 0.95);

#task 6
data = read.csv("E:/Data1.txt", header = FALSE);
x = data$V1;
shapiro.test(x); 
y = cut(x,breaks = c(0,2.5,5,7.5,10)); 
m = table(y);
chisq.test(m) 




