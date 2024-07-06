# bootstrap
library(boot)
data <- c(90, 77, 100, 83, 64, 78, 92, 73, 122, 96, 60, 85, 86, 108, 70, 139, 56, 94, 84, 111, 93, 120, 70, 92, 100, 124, 59, 112, 79)
set.seed(11)
mean.fun <- function(comp,i)+{x<-mean(comp[i])}
b <- boot(data,mean.fun,R=2000)
ci <- boot.ci(b,conf=0.95,type=c("norm","basic","perc","bca"))
ci

# 1
library(Rmisc)
food <- c(7.42,6.29,5.83,6.5,8.34,9.51,7.1,6.8,5.9,4.89,6.5,5.52,7.9,8.3,9.6)
m <- mean(food)
sd <- sd(food)
n <- 15
sde <- sd/sqrt(n)
moferr<-qt(0.975,n-1)*sd/sqrt(n)
lcl<-m-moferr
ucl<-m+moferr
CI(food, ci=0.95)

# 3
library(Rmisc)
prop.test(24,3900)

# 4
prop.test(x=c(306,391), n=c(564,637), conf.level = 0.95)

# 5
grad <- c(15,7,15,10,5,5,2,3,12,16,15,37,8,14,10,18,3,25,15,5,5)
no_grad <- c(6,8,15,6,5,14,10,10,12,5)
t.test(grad,no_grad,conf.level = 0.95, var.equal = TRUE)

#6
before <- c(158,189,202,353,416,426,441)
after <- c(284,214,101,227,290,176,290)
t.test(before,after,conf.level = 0.95,paired = TRUE)