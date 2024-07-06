# EXAMPLE 1: Work involvement & Job satisfaction
wi <-c(43,55,67,38,49,70,80,62,73,83)
js <-c(4,5,6,4,5,7,9,5,6,9)
cor.test(wi, js, method = c("pearson"),alternatives = "two.sided")

plot(js, wi, main="Scatterplot", xlab="Job Satisfaction ", ylab="Work Involvement", pch=19)


# EXAMPLE 2: 10 automatic vending machines of different age VS maintenance cost
age<-c(4,3,2,4,3,5,5,4,2,3)
mcost<-c(148,145,128,143,133,159,154,142,118,127)
dataf <- data.frame(age,mcost)
model <- lm(mcost ~ age, data = dataf)
summary(model)

plot(age,mcost,col = "blue",main = "Maintenance cost & Age Regression",
     abline(model),cex = 1.3,pch = 16,xlab = "Age in years",ylab = "Maintenance cost in x100 baht")

# EXAMPLE 3:Height (cm.) estimation (cm.) from forearm length (cm.) in females
height<-c(165.8,169.8,170.7,170.9,157.5,165.9,158.7,166,158.7,161.5,167.3,167.4,159.2,170,166.3,169,156.2,159.6,155,161.1,170.3,167.8,163.1,165.8,175.4,159.8,166,161.2,160.4,164.3,165.5,167.2,167.2)
forearm<-c(28.1,29.1,29.5,28.2,27.3,29,27.8,26.9,27.1,27.8,27.3,30.1,27.3,30.9,28.8,28.8,25.6,25.4,26.6,26.6,29.3,28.6,26.9,26.3,30.1,27.1,28.1,29.2,27.8,27.8,28.6,27.1,29.7)
dataf <- data.frame(forearm,height)
model <- lm(height ~ forearm, data = dataf)
summary(model)

plot(forearm,height,col = "blue",main = "Height & Forearm Regression",
     abline(model),cex = 1.3,pch = 16,xlab = "Forearm in cm",ylab = "Height in cm")

res <- resid(model)
plot(fitted(model), res)

# Multiple Linear Regression
# EXERCISE 1: PIQ (score) estimation from brain (count/10,000), height (inches), weight(pounds)
piq<-c(124,150,128,134,110,131,98,84,147,124,128,124,147,90,96,120,102,84,86,84,134,128,102,131,84,110,72,124,132,137,110,86,81,128,124,94,74,89)
brain<-c(81.69,103.84,96.54,95.15,92.88,99.13,85.43,90.49,95.55,83.39,107.95,92.41,85.65,87.89,86.54,85.22,94.51,80.8,88.91,90.59,79.06,95.5,83.18,93.55,79.86,106.25,79.35,86.67,85.78,94.96,99.79,88,83.43,94.81,94.94,89.4,93,93.59)
height<-c(64.5,73.3,68.8,65,69,64.5,66,66.3,68.8,64.5,70,69,70.5,66,68,68.5,73.5,66.3,70,76.5,62,68,63,72,68,77,63,66.5,62.5,67,75.5,69,66.5,66.5,70.5,64.5,74,75.5)
weight<-c(118,143,172,147,146,138,175,134,172,118,151,155,155,146,135,127,178,136,180,186,122,132,114,171,140,187,106,159,127,191,192,181,143,153,144,139,148,179)
dataf <- data.frame(piq,brain,height,weight)
model <- lm(piq ~ brain+height+weight, data = dataf)
summary(model)

library(MASS)
step.model <- stepAIC(model, direction = "both", trace = FALSE)
summary(step.model)

library(car)
durbinWatsonTest(step.model)

res <- resid(step.model)
plot(fitted(step.model), res)

qqnorm(res)
qqline(res)

shapiro.test(res)

out<-cooks.distance(step.model)
mean(out)

library(car)
vif(step.model)
fmodel <- aov(piq ~ brain+height+weight, data = dataf)
fmodel
rmodel <- aov(piq ~ brain+height, data = dataf)
rmodel

