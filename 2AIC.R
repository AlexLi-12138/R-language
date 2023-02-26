#Five cross terms to find the best model using aic

Rent <- read.csv("mansion.csv",header=T)
head(Rent)

fit <- lm(rent~access+l.dummy+area+age,data=Rent)
AIC(fit)

#The five variables are two by two to form ten interacted models
fit1 <- lm(rent~area*(age+access+l.dummy+d.dummy)+age*(access+l.dummy+d.dummy)+access*(l.dummy+d.dummy)+l.dummy*d.dummy,data=Rent)
summary(fit1)
#Stepwise regression analysis yields optimal regression with interacted models
tstep <- step(fit1)


fit2 <- lm(formula = rent ~ area + age + access + l.dummy + d.dummy + 
             area:age + area:access + access:l.dummy, data = Rent)  
summary(fit2)  
#Improving the results of Stepwise regression analysis
drop1(tstep)

fit3 <- lm(formula = rent ~ area + access + l.dummy + area:age , data = Rent)  
summary(fit3)
AIC(fit3)
