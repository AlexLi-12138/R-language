#The VAR model is used to predict the number of COVID-19 cases per prefecture.
#https://www3.nhk.or.jp/news/special/coronavirus/data-widget/ 

library(MASS)
library(zoo)
library(sandwich)
library(strucchange)
library(urca)
library(lmtest)
library(vars)

# prepocessing data
covid <- read.csv("covid_2.csv", header=T, fill = T, fileEncoding = "UTF-8")
head(covid, 5)
#包含日语的文件遇到bug：列的数目大于名字 说明逗号分割文件形式不对 改存为csv逗号分割格式 然后用notepad变为utf编码 加filencoding


#import data as mydata
mydata <- data.frame(covid$Saitama, covid$Tokyo,covid$Chiba,covid$Kanagawa)
head(mydata, 5)
matplot(mydata,type="l", lty=1)

#VAR model and select best lag order
VARselect(mydata, lag.max = 100, type = "const",season = 7, exogen = NULL)
var1 <- VAR(mydata, p = 10, type="const", season = 7)
var2 <- VAR(mydata, p = 4, type="const",season = 7)
var3 <- VAR(mydata, p = 20, type="const",season = 7)
summary(var1)

#predict future 21 days
var.predict<- predict(var1,n.ahead=21,ci=0.95)
plot(var.predict)

#use train data set to predict the last 21 days
train <- mydata[1:659,]
test <- mydata[660:680,]
result <- predict(var1,train,n.ahead=21)
plot(result)

#accuracy of prediction
res <- result$fcst
r1 <- res$covid.Saitama
r2 <- res$covid.Tokyo
r3 <- res$covid.Chiba
r4 <- res$covid.Kanagawa
rr <- cbind(r1[,1],r2[,1],r3[,1],r4[,1])
m <- abs(rr-test)/rr
mean(m$covid.Saitama)
mean(m$covid.Tokyo)
mean(m$covid.Chiba)
mean(m$covid.Kanagawa)



library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(ggsci)
library(knitr)
library(gcookbook)


#画实际值与预测值的对比图
saitama <- read.csv("saitama.csv", header=T, fill = T, fileEncoding = "UTF-8")
tokyo <- read.csv("tokyo.csv", header=T, fill = T, fileEncoding = "UTF-8")
chiba <- read.csv("chiba.csv", header=T, fill = T, fileEncoding = "UTF-8")
kanagawa <- read.csv("kanagawa.csv", header=T, fill = T, fileEncoding = "UTF-8")
p1<-ggplot(saitama,aes(Date,people,group=group,color=group,shape=group))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3 ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2 <- ggplot(tokyo,aes(Date,people,group=group,color=group,shape=group))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3 ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3<-ggplot(chiba,aes(Date,people,group=group,color=group,shape=group))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3 ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p4<-ggplot(kanagawa,aes(Date,people,group=group,color=group,shape=group))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3 ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1+ggtitle("saitama")+theme(plot.title = element_text(hjust = 0.5))
p2+ggtitle("tokyo")+theme(plot.title = element_text(hjust = 0.5))
p3+ggtitle("chiba")+theme(plot.title = element_text(hjust = 0.5))
p4+ggtitle("kanagawa")+theme(plot.title = element_text(hjust = 0.5))


