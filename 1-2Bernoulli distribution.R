#If Xi follows a Bernoulli distribution with probability of success p, 
#it corresponds to the case where ¦Ì = p and ¦Ò2 = p(1 - p). 
#Fix p = 0.5 and compare the actual distribution of X¡¥ with the approximate distribution as in all questions.

data1<-rbinom(10000,1,0.5)

#Set the sampling sample size(which can be change among 50,100,500,1000)
n<-50
#sampling times
times<-1000
#Sampling the sample and taking the mean value
yn <-c()
for(i in 1:times){
  sampledata <- sample(data1,n)
  yn <- append(yn,length(sampledata[sampledata>0]))}

np <- n*mean(data1)
sig <- sqrt(0.5*0.5*n)
N <- rnorm(times,mean=np,sd=sig)

#ks.test: when D close to 0, and p>0.05, data is normal distribution
ks.test(yn, N)

savedata <- c()
for(i in 1:100){
  sample1 <-  ks.test(yn,rnorm(times,mean=np,sd=sig))
  savedata <- append(savedata,sample1[["p.value"]])}
length(savedata[savedata>0.05])

hist(N)
hist(yn)
