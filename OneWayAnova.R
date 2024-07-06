# One way anova
head_dia <- c(14.15,14.30,14.85,14.60,14.25,15.10,14.65,14.45,14.85,13.65,13.45,14.20,12.65,14.35,13.50,12.80,14.15,13.90,13.65,13.60,13.20,13.20,14.05,13.80)
boys <- c('a','a','a','a','a','a','a','a','t','t','t','t','t','t','t','t','c','c','c','c','c','c','c','c')
aov.model <- aov(head_dia ~ boys)
summary(aov.model)

TukeyHSD(aov.model)

library(car)
leveneTest(aov.model ,center=mean)