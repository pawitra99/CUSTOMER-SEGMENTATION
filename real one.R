#set working directory
setwd("C:/Users/Guest/Documents/R")

#load packages
library(ggplot2) # for plotting graph
library(NbClust) # for finding no.of clusters

#read data
read.table('Customer Data.csv', header=1, sep=',')

#pass table to 'data' variable
data <- read.table('Customer Data.csv', header=1, sep=',')

#view structure of dataset
str(data)

#view summary of dataset
summary(data)

#take 1st (Age) and 3rd (Amount purchased)column of the data -> bisa diganti berdasarkan analisa yang diinginkan
data.new <- data [,c(1,3)]
data.new1 <- data [,c(1,4)]
data.new2 <- data [,c(2,3)]

#plot data
ggplot(data, aes(Age, Amount.purchased))+geom_point()
ggplot(data, aes(Age, Total.Visits)) +geom_point()
ggplot(data, aes(Income.K., Amount.purchased)) +geom_point()

#scale the data
scaled_data <- scale(data.new)
scaled_data1 <- scale(data.new1)
scaled_Data2 <- scale(data.new2)

#finding optimal no. of clusters using optimal rule
k <- NbClust(scaled_data, diss=NULL, distance = "manhattan", min.nc=2, max.nc=12, 
             method = "kmeans", index = "all")

q <- NbClust(scaled_data1, diss=NULL, distance = "manhattan", min.nc=2, max.nc=12, 
             method = "kmeans", index = "all")

r <- NbClust(scaled_Data2, diss=NULL, distance = "manhattan", min.nc=2, max.nc=12, 
             method = "kmeans", index = "all")


#according to optimal rule, no.of clusters suggested is 2
clust_no <- kmeans(scaled_data, 3, nstart=20, iter.max = 20)

clust_no1 <- kmeans(scaled_data1, 3 ,nstart=20, iter.max=20)

clust_no2 <- kmeans(scaled_Data2, 2, nstart=20, iter.max=20)

#to see the no.of records in each cluster
clust_no$size

clust_no1$size

clust_no2$size

#to see the centroids of scaled data
clust_no$centers

clust_no1$centers

clust_no2$centers

#shows where each records falls into which cluster
clust_no$cluster
table(clust_no$cluster)

clust_no1$cluster
table(clust_no1$cluster)

clust_no2$cluster
table(clust_no2$cluster)


#plotting data of Age vs Amount purchased after clustering
clust_no$cluster <- as.factor(clust_no$cluster)
ggplot(data, aes(Age, Amount.purchased, color = clust_no$cluster)) +geom_point() 

clust_no1$cluster <- as.factor(clust_no1$cluster)
ggplot(data, aes(Age, Total.Visits, color = clust_no1$cluster)) +geom_point()

clust_no2$cluster <- as.factor(clust_no2$cluster) 
ggplot(data, aes(Income.K., Amount.purchased, color = clust_no2$cluster)) +geom_point()


