#Principal component analysis to discuss differences across counties

data <- read.csv("2019.csv",header = T,as.is = TRUE,row.names = 1,fileEncoding = "UTF-8")
head(data)
dim(data)

data.pr <- princomp(data, cor = T)
summary(data.pr)
data.pr$loadings
loadings(data.pr)
biplot(data.pr)
library(ggplot2)
library(factoextra)
fviz_eig(data.pr,addlabels = T)


Score <- data.pr$score[,c(1,2)]
Score
plot(Score, cex=1)


identify(Score, labels=1:dim(Score)[1])

head(USArrests)
names(USArrests)
names(Batting)
class(Batting$playerID)

prus <- princomp(USArrests,cor = T)
biplot(prus)

names(data.pr)
