# Two way ANOVA
# EXAMPLE 2: User Friendliness
library(MASS)
library(reshape2)
library(reshape)
user.friend <- data.frame( sys = c("A", "B", "C"), 
                           AG1 = c(6.5,7.5,8.0), 
                           AG2 = c(7.0,7.0,8.5), 
                           AG3 = c(6.0,7.5,8.0), 
                           AG4 = c(7.5,8.0,8.0), 
                           AG5 = c(7.0,7.0,7.5),
                           AG6 = c(6.5,8.0,7.5),
                           AG7 = c(7.0,7.5,7.0), 
                           AG8 = c(6.5,7.0,6.0))
muser.friend<- melt(user.friend, id.var="sys")
summary(aov(value ~ sys + variable, data=muser.friend))

user.friend2 <- data.frame( age = c("<20", "20-25", "26-30", "31-35", 
                                     "36-40", "41-45", "46-50", ">50"), 
                           sysA = c(6.5,7.0,6.0,7.5,7.0,6.5,7.0,6.5), 
                           sysB = c(7.5,7.0,7.5,8.0,7.0,8.0,7.5,7.0), 
                           sysC = c(8.0,8.5,8.0,8.0,7.5,7.5,7.0,6.0))
muser.friend2 <- melt(user.friend2, id.var="age")

library(car)
leveneTest(value ~ variable, muser.friend ,center=mean)
leveneTest(value ~ variable, muser.friend2 ,center=mean)

score <- c(6.5,7.0,6.0,7.5,7.0,6.5,7.0,6.5,7.5,7.0,7.5,8.0,7.0,8.0,7.5,7.0,8.0,8.5,8.0,8.0,7.5,7.5,7.0,6.0)
sys <- c("A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C")
aov.model <- aov(score ~ sys)
TukeyHSD(aov.model)

-----------------------------------------------------------------------------------------
# Example 4: Automobile Interior Noise Level
noise.levels <- data.frame(Speed = c("Idle","60mph","0-60mph"),
                          Chrysler = c(41,65,76),
                          BMW = c(45,67,72),
                          Ford = c(44,66,76),
                          Chevy = c(45,66,77),
                          Subaru = c(46,76,64))
mnoise.levels<-melt(noise.levels, id.var="Speed")
summary(aov(value ~ Speed + variable, data=mnoise.levels))

leveneTest(value ~ variable, mnoise.levels ,center=mean)

score <- c(41,45,44,45,46,65,67,66,66,76,76,72,76,77,64)
speed <- c("Idle","Idle","Idle","Idle","Idle","60mph","60mph","60mph","60mph","60mph","0-60mph","0-60mph","0-60mph","0-60mph","0-60mph")
aov.model <- aov(score ~ speed)
TukeyHSD(aov.model)

