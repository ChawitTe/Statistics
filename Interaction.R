# EXAMPLE 3: No. of days absent from work Factors: No. of training days and department (interaction)
absent <-c(3.0,4.5,4.0,5.0,4.5,4.0,2.5,3.0,3.5,2.0,2.0,3.0,
           2.0,2.5,2.0,1.0,3.0,2.5,1.0,3.0,1.5,5.0,4.0,2.5,
           2.5,1.0,1.5,0.0,1.5,2.0,3.5,3.5,4.0,4.0,4.5,5.0)
dept <- as.factor(c(rep("Warehouse",3), rep("Commow",3), rep("Bigw",3), rep("Refrigw",3)))
train <- as.factor(c(rep("1-20",12), rep("21-50",12), rep(">50",12)))
mdataframe <- data.frame(absent,dept,train)

library(car)
leveneTest(absent ~ dept * train, data=mdataframe ,center=mean)

res.aov3 <- aov(absent ~ dept * train, data = mdataframe)
summary(res.aov3)

model.tables(res.aov3, type="means", se = TRUE)


with(mdataframe, interaction.plot(train, dept, absent, fun = mean, main = "Interaction Plot"))


TukeyHSD(res.aov3)

------------------------------------------------------------------------------------------------
# EXAMPLE 6: Delivery times(indays)
delivery_times <- c(8,8,10,13,13,14,13,12,11,10,12,14,7,10,10,13,
                    14,9,14,11,9,9,7,8,8,9,11,12,8,13,9,12,
                    10,15,10,7,12,10,10,11,12,10,13,10,7,5,6,5,
                    8,7,13,10,6,10,12,8,10,11,7,10,8,5,11,4,
                    17,12,9,10,15,12,12,10,14,13,10,12,14,13,8,11)
clinic <- as.factor(c(rep("Clinic-A",4), rep("Clinic-B",4), rep("Clinic-C",4), rep("Clinic-D",4)))
supplier <- as.factor(c(rep("Supplier1",16), rep("Supplier2", 16), rep("Supplier3",16), rep("Supplier4",16), rep("Supplier5",16)))
mdataframe2 <- data.frame(delivery_times ,clinic ,supplier)

library(car)
leveneTest(delivery_times ~ clinic * supplier, data=mdataframe2 ,center=mean)

res.aov6 <- aov(delivery_times ~ clinic * supplier, data=mdataframe2)
summary(res.aov6)

model.tables(res.aov6, type="means", se = TRUE)

TukeyHSD(res.aov6)
