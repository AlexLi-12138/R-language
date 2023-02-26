################################################################
library(mclust)
library(MASS)
library(factoextra)
library(cluster)
library(ggplot2)



data("USJudgeRatings")
?USJudgeRatings
df <- USJudgeRatings
df <- na.omit(df)
df <- scale(df)
head(df)

fviz_nbclust(df, kmeans, method = "wss")


#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 43)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

# Set random seeds to make results reproducible
set.seed(1)

# Call the kmeans clustering algorithm k = 7
km <- kmeans(df, centers = 7, nstart = 25)

# View Results
km

#plot results of final k-means model
fviz_cluster(km, data = df)

#find means of each cluster
aggregate(USJudgeRatings, by=list(cluster=km$cluster), mean)

#add cluster assigment to original data
final_data <- cbind(df, cluster = km$cluster)

#view final data
head(final_data)




