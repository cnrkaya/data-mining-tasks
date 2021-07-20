###############################
#author: Caner Kaya - K-6211
#Clustering task

#Objective : Determine the best clustering according to the
#evaluation methods presented during the laboratory

###############################


library(cluster)
library(dbscan)
library(fpc)
library(factoextra)

#wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_dsC <- wineRed_ds[,-12]
str(wineRed_dsC)

head(wineRed_dsC)
nrow(wineRed_dsC)

# 'data.frame':	1599 obs. of  11 variables:
#   $ fixed.acidity       : num  7.4 7.8 7.8 11.2 7.4 7.4 7.9 7.3 7.8 7.5 ...
# $ volatile.acidity    : num  0.7 0.88 0.76 0.28 0.7 0.66 0.6 0.65 0.58 0.5 ...
# $ citric.acid         : num  0 0 0.04 0.56 0 0 0.06 0 0.02 0.36 ...
# $ residual.sugar      : num  1.9 2.6 2.3 1.9 1.9 1.8 1.6 1.2 2 6.1 ...
# $ chlorides           : num  0.076 0.098 0.092 0.075 0.076 0.075 0.069 0.065 0.073 0.071 ...
# $ free.sulfur.dioxide : num  11 25 15 17 11 13 15 15 9 17 ...
# $ total.sulfur.dioxide: num  34 67 54 60 34 40 59 21 18 102 ...
# $ density             : num  0.998 0.997 0.997 0.998 0.998 ...
# $ pH                  : num  3.51 3.2 3.26 3.16 3.51 3.51 3.3 3.39 3.36 3.35 ...
# $ sulphates           : num  0.56 0.68 0.65 0.58 0.56 0.56 0.46 0.47 0.57 0.8 ...
# $ alcohol             : num  9.4 9.8 9.8 9.8 9.4 9.4 9.4 10 9.5 10.5 ...


#calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

# BASE METHOD
#Algorithm: Kmeans - 6 cluster 
#Distance Metric: Euclidian Disctance
#Scaling: No
set.seed(123)
distC = dist(wineRed_dsC)
card.kmeans = kmeans(distC,6)
res0 = table(wineRed_ds$quality,card.kmeans$cluster )
res0

#Accuracy
accuracyCalc(res0,1)  # 0.4865541

# Average Silhouette 
ss <- silhouette(card.kmeans$cluster, distC)
mean(ss[, 3]) 

# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement).
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, card.kmeans$cluster)
clust_stats$corrected.rand  #0.002888379


################ KMEANS ################

### TEST: check success after data scaling

#Algorithm: Kmeans - 6 cluster 
#Distance Metric: Euclidian Distance
#Scaling: YES
set.seed(123)
wineRed_dsC_ = scale(wineRed_dsC)
distC = dist(wineRed_dsC_, method = "euclidian" )
card.kmeans = kmeans(distC,6)
res1 = table(wineRed_ds$quality,card.kmeans$cluster )
res1
accuracyCalc(res1,1) #0.5203252

# Average Silhouette 
ss <- silhouette(card.kmeans$cluster, distC)
mean(ss[, 3])   #0.1505423

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, card.kmeans$cluster)
clust_stats$corrected.rand  #.07198369

#As we can see, scaling the data increased the accuracy.
#silhouette and rand index scores decreased due to scaling.


### TEST: check the succes of the different distance measures

#Manhattan Distance
#Algorithm: Kmeans - 6 cluster 
#Distance Metric: Manhattan Distance
#Scaling: YES
set.seed(123)
distC = dist(wineRed_dsC_, method = "manhattan" )
card.kmeans = kmeans(distC,6)
res2 = table(wineRed_ds$quality,card.kmeans$cluster )
res2
accuracyCalc(res2,1)  #0.5259537

# Average Silhouette 
ss <- silhouette(card.kmeans$cluster, distC)
mean(ss[, 3])   #0.1419467

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, card.kmeans$cluster)
clust_stats$corrected.rand  #0.06670274

# Using Manhattan distance metric yielded results close to Euclidean distance

#Canberra Distance
#Algorithm: Kmeans - 6 cluster 
#Distance Metric: Canberra Distance
#Scaling: YES
set.seed(123)
distC = dist(wineRed_dsC_, method = "canberra" )
card.kmeans = kmeans(distC,6)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)  #0.5434647

# Average Silhouette 
ss <- silhouette(card.kmeans$cluster, distC)
mean(ss[, 3])   #0.09938806

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, card.kmeans$cluster)
clust_stats$corrected.rand  #0.05377876

#Canberra Distance better than the others according to accuracy

### TEST: check the optimum cluster size

#Elbow Method
set.seed(123)
fviz_nbclust(wineRed_dsC_, kmeans, method = "wss")

#Average Silhouette Method
set.seed(123)
fviz_nbclust(wineRed_dsC_, kmeans, method = "silhouette")

#According to the elbow and average method, the ideal cluster numbers are seen as 6 and 8.

#Algorithm: Kmeans - 8 cluster 
#Distance Metric: Canberra Distance
#Scaling: YES
set.seed(123)
distC = dist(wineRed_dsC_, method = "canberra" )
card.kmeans = kmeans(distC,8)
res5 = table(wineRed_ds$quality,card.kmeans$cluster )
res5
accuracyCalc(res5,1)  #0.5559725

# Average Silhouette 
ss <- silhouette(card.kmeans$cluster, distC)
mean(ss[, 3])   #0.08727489

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, card.kmeans$cluster)
clust_stats$corrected.rand  #0.05485157

#With these parameters, the best results in the kmeans algorithm were obtained.

################## DBSCAN ##################

#### TEST : data scaling 

#normal data
dbscan::kNNdistplot(wineRed_dsC, k=5)
abline(h=7, lty="dashed")

#scaled data
dbscan::kNNdistplot(wineRed_dsC_, k=5)
abline(h=2.1, lty="dashed")


#### TEST : data scaling 
#Algorithm: DBSCAN 
#Epsilon: 13   MinimumPoints: 5
#Scaling: No
set.seed(123)
wine.dbscan <- dbscan(wineRed_dsC, eps=7,MinPts = 5)
#compare clusters with original class labels
res6<-table(wineRed_ds$quality, wine.dbscan$cluster)
accuracyCalc(res6,1)  #0.4258912

# very bad result compared to k means

#Algorithm: DBSCAN 
#Epsilon: 2.1   MinimumPoints: 5
#Scaling: Yes
set.seed(123)
wine.dbscan <- dbscan(wineRed_dsC_, eps=2.3, MinPts=5)
#compare clusters with original class labels
res7<-table(wineRed_ds$quality, wine.dbscan$cluster)
accuracyCalc(res7,1)  #0.4258912

#scaling did not change the result


#Algorithm: DBSCAN 
#Epsilon: 2.1   MinimumPoints: 10
#Scaling: No
set.seed(123)
wine.dbscan <- dbscan(wineRed_dsC, eps=2.3, MinPts=10,)
#compare clusters with original class labels
res8<-table(wineRed_ds$quality, wine.dbscan$cluster)
accuracyCalc(res8,1)  #0.4490306

#increasing the minimumPoints did not work well


#Algorithm: DBSCAN 
#Epsilon: 2.1   MinimumPoints: 3
#Scaling: No
set.seed(123)
wine.dbscan <- dbscan(wineRed_dsC, eps=2.1, MinPts=3)
#compare clusters with original class labels
res9<-table(wineRed_ds$quality, wine.dbscan$cluster)
accuracyCalc(res9,1)  #0.5021889

#decreasing the minimumPoints is better but minPts < 3 is not make sense
#because minPts = 2, the result will be the same as of hierarchical clustering with the single link metric,
#and minPts = 1 every point on its own will already be a cluster.



################## HIERARCHICAL CLUSTERING ##################


# Calculation of a distance matrix
distM = dist(wineRed_dsC)
distT = as.matrix(distM)


### TEST: Hierarchical clustering for different linkage methods

red_wine.hc_centroid <- hclust(distM, method="centroid")

#### 1. Complete method  #### 
#k = 6

red_wine.hc_complete <- hclust(distM, method="complete")

wine.complete<- cutree(red_wine.hc_complete, k=6)
res10 = table(wineRed_ds$quality, wine.complete)

#dendogram
plot(red_wine.hc_complete, hang = -1, labels=wineRed_ds$quality)
rect.hclust(red_wine.hc_complete, k=6, border=1:4)

# Accuracy
accuracyCalc(res10, 1) #0.478424

# Average Silhouette 
ss <- silhouette(wine.complete, distC)
mean(ss[, 3])   #0.4388105

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, wine.complete)
clust_stats$corrected.rand  # -0.002913121


#### 2. Single method #### 
#k = 6

red_wine.hc_single <- hclust(distM, method="single")

wine.single<- cutree(red_wine.hc_single, k=6)
res11 = table(wineRed_ds$quality, wine.single)

#dendogram
plot(red_wine.hc_single, hang = -1, labels=wineRed_ds$quality)
rect.hclust(red_wine.hc_single, k=6, border=1:4)

# Accuracy
accuracyCalc(res11, 1) #0.4290181

# Average Silhouette 
ss <- silhouette(wine.single, distC)
mean(ss[, 3])   #0.4044111

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, wine.single)
clust_stats$corrected.rand  # 0.001952802

#### 3. Average method #### 
#k = 6

red_wine.hc <- hclust(distM, method="average")

wine.avg<- cutree(red_wine.hc, k=6)
res12 = table(wineRed_ds$quality, wine.avg)

#dendogram
plot(red_wine.hc, hang = -1, labels=wineRed_ds$quality)
rect.hclust(red_wine.hc, k=6, border=1:4)

# Accuracy
accuracyCalc(res12, 1) #0.4796748

# Average Silhouette 
ss <- silhouette(wine.avg, distC)
mean(ss[, 3])   #0.5540213

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, wine.avg)
clust_stats$corrected.rand  #  -0.01161044

#### 4. Centroid method #### 
#k = 6

red_wine.hc_centroid <- hclust(distM, method="centroid")

wine.centroid<- cutree(red_wine.hc_centroid, k=6)
res13 = table(wineRed_ds$quality, wine.centroid)

#dendogram
plot(red_wine.hc_centroid, hang = -1, labels=wineRed_ds$quality)
rect.hclust(red_wine.hc_centroid, k=6, border=1:4)

# Accuracy
accuracyCalc(res13, 1) #0.4828018

# Average Silhouette 
ss <- silhouette(wine.centroid, distC)
mean(ss[, 3])   #0.538293

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, wine.centroid)
clust_stats$corrected.rand  #  -0.01419141

### TEST: data scaling

distM = dist(wineRed_dsC_)
distT = as.matrix(distM)

#centroid method
#k = 6

red_wine.hc_centroid <- hclust(distM, method="centroid")

wine.centroid<- cutree(red_wine.hc_centroid, k=6)
res14 = table(wineRed_ds$quality, wine.centroid)

#dendogram
plot(red_wine.hc_centroid, hang = -1, labels=wineRed_ds$quality)
rect.hclust(red_wine.hc_centroid, k=6, border=1:4)

# Accuracy
accuracyCalc(res14, 1) #0.4277674

# Average Silhouette 
ss <- silhouette(wine.centroid, distC)
mean(ss[, 3])   #-0.007154971

# Rand index
clust_stats<-cluster.stats(d=distC, wineRed_ds$quality, wine.centroid)
clust_stats$corrected.rand  #  0.001912832

#Scaling the data negatively impacted the result


##################### CONCLUSION ##################### 

# Clustering was done using 3 different algorithms

# 1. KMeans
# After scaling, it was seen that accuracy increased
# 3 different distance metrics were used: 
# Canberra performed best while Euclidian and Manhattan are showed similar results
# Elbow and average silhouette methods were used to determine the optimum cluster size.
# Optimum cluster size was found to be 8
# The best result is 0.5559725 accuracy with 8 cluster, canberra distance and scaled data.

# 2.DBSCAN
# Scaling the data had no positive effect on the result
# The k-Nearest Neighbor Distance was drawn to find the epsilon and minumumpoint parameters, 
# but the results were very bad compared to kmeans.
# The best result is 0.5021889 accuracy with Epsilon: 2.1 , MinimumPoints: 3 and no scaling

# 3.HIERARCHICAL CLUSTERING
# The silhouette scores of this method were found to be high but its accuracy was low.
# 4 different distance metrics used succes ranking sorting by accuracy: 
# Centroid > Average >  Complete > Single 
# Scaling the data negatively impacted the result
# The best result is 
# accuracy: 0.4828018, avg silhouette:0.538293,random index:-0.007154971 with centroid metric,k=6

#Based on accuracy, the most successful result was obtained with the kmeans method.



