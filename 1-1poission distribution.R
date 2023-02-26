# When Xi follows a Poisson distribution with parameter ¦Ë, ¦Ì = ¦Ë, ¦Ò 2 = ¦Ë is equivalent to the following. 
#When ¦Ë = 5 and n is varied from 50, 100, 500, 1000, visually confirm that the shape of 
#the distribution of X¡¥ approaches the normal distribution N(¦Ë, ¦Ë/n).
  
#Generate x1~xn conforming to Poisson distribution (assume n=1000)
data1<-rpois(10000,5)


#Set the sampling sample size(which can be change among 50,100,500,1000)
n<-500
#sampling times
times<-10000
#Sampling the sample and taking the mean value
xmean<-c()
for(i in 1:times){
  sampledata <- sample(data1,n)
  xmean <- append(xmean,mean(sampledata))
}

#library(ggplot2)
#ggplot(data = NULL,aes(xmean))+geom_histogram(stat = "density")
hist(xmean)

#shapiro test: when w close to 1, p>0.05,data is normal distribution
#shapiro test request sample size <5000, so sampling xmean
shapiro.test(sample(xmean,5000))

mu<-mean(data1)
sig <- sd(data1)/sqrt(n) 

N <- rnorm(times,mean=mu,sd=sig)
#shapiro.test(sample(N,5000))
hist(N)
#ggplot(data = NULL,aes(N))+geom_histogram(stat = "density")

#ks.test: when D close to 0, and p>0.05, data is normal distribution
ks.test(xmean, N)

savedata <- c()
for(i in 1:100){
  sample1 <-  ks.test(xmean,rnorm(times,mean=mu,sd=sig))
  savedata <- append(savedata,sample1[["p.value"]])}
length(savedata[savedata>0.05])


