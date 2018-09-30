
####Data Science: Third Project
####Author: William Duncan
####Last edited: 9/30/2018

library(readxl)
moviedata <- read_excel("C:/Users/WilliamDuncan/Desktop/Econ PhD/Introduction to Data Science/Week 5/moviedata.xlsx")

# Determine number of clusters
wss <- (nrow(moviedata)-1)*sum(apply(moviedata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(moviedata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(moviedata, 5) # 5 cluster solution
# get cluster means 
aggregate(moviedata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
moviedata <- data.frame(moviedata, fit$cluster) 

# Ward Hierarchical Clustering
moviedata1 <- moviedata[1:1000,]
d <- dist(moviedata1, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red") 

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit1 <- pvclust(moviedata1, method.hclust="ward",
               method.dist="euclidean")
plot(fit1) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit1, alpha=.95) 

# Model Based Clustering
library(mclust)
fit2 <- Mclust(moviedata1)
plot(fit2) # plot results 
 # display the best model 


# K-Means Clustering with 5 clusters
fit3 <- kmeans(moviedata1, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(moviedata1, fit3$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(moviedata1, fit3$cluster) 


