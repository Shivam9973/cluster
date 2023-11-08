# use built in dataset for r clustering
data("iris")
names(iris)

# create subset of data
new_data<-subset(iris,select = c(-Species))
new_data

#here we can see 3 clusters of size 33,21,96 are formed where 3 is provided as cluster size
cl<-kmeans(new_data, 3)
cl

# use elbow method to find optimal number of clusters 
data<-new_data
wss<-sapply(1:15, function(k){kmeans(data, k)$tot.withinss})
wss

# plot graph
plot(1:15, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of cl sters K", ylab = "Total within-clusters sum of squares")

# install package
install.packages("cluster")
library(cluster)

# cluster plot
clusplot(new_data, cl$cluster, color=TRUE, shade=TRUE, labels = 2, lines = 0)
cl$cluster

# centred points of clusters are as per following
cl$centers

# cluster dendogram
clusters<-hclust(dist(iris[, 3:4]))
plot(clusters)
clustercut<-cutree(clusters, 3)
table(clustercut, iris$Species)
# here we can see that first cluster only have setosa species and second cluster contains 21 versicolor and 50 virginica species as the difference between their petal measurements is less. Third cluster contains only versicolor species


# b
# Installing the Packages
install.packages("ClusterR")
install.packages("cluster")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("fpc")
install.packages("dbscan")
install.packages("purrr")
# Loading the package
library(ClusterR)
library(cluster)
library(tidyverse)
library(purrr)
library(factoextra)
library(fpc)
library(dbscan)
#importing dataset wholesale customers data
data <-read.csv(file.choose(),header=T)
# the csv dataset file is selected
#displaying initial 6 records of dataset
head(data)
# firt 6 records of file are shown.
#omitting na values from dataset
data<-na.omit(data)
# na values are omitted to avoid null values error
#data$Channel = as.factor(data$Channel)
#data$Region = as.factor(data$Region)
#removing channel and Region column
data1 = subset(data, select = -c(Channel,Region) )
head(data1)
# the categorical columns channel and region are dropped and the data first 6 values are displayed
#plotting elbow graph for 1 to 15 values
#scalling values
scal <- scale(data1)
scal[0:10]
#calculating the total within distance
wss <- sapply(1:15, function(k){kmeans(scal, k)$tot.withinss})
wss
#plotting the elbow plot
plot(1:15, wss,type='b')
# scaling the values and calculating the within sum of squares and plotting the elbow graph
# from elbow method the optimum clusters can be determined as 5
#k-means algorithm for 5 clusters
kmean <- kmeans(scale(data1), centers = 3)
kmean
# displaying the cluster centroids and to which clusters do each of the records in the dataset belong to
# Cluster belonging to each observation and centers
kmean$cluster
kmean$centers
# plotting the clusters in circular shape and another plot with boundary elements
# plot the clusters from k-means algorithm
# non definite shape cluster
fviz_cluster(kmean, data = scale(data1), geom = c("point"),ellipse = TRUE)
#circular (euclid) cluster
fviz_cluster(kmean, data = scale(data1), geom = c("point"),ellipse.type="euclid")
# plotting a cusplot
clusplot(scale(data1),
        kmean$cluster,
        lines = 0,
        shade = TRUE,
        color = TRUE,
        labels = 2,
        main = "Cluster",
)
