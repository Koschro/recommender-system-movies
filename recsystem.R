#####Recsystem######

#First we load the data concerning the movies ratings and the features about the movies
library(recommenderlab)

movielens_data <- read.csv("~/RapidMiner/ml-100k/movielens_data.csv", sep=";")
View(movielens_data)

movies_rating = as(movielens_data,"realRatingMatrix")

itemsData <- read.delim("~/R/Recommender Systems/itemsData.txt", comment.char="#")
View(itemsData)
itemsData = itemsData[,-2]

#extract the movie names

movie_names=rep(NA)

for (i in 1:nrow(movielens_data)) {
  movie_names[i] = as.character(itemsData[movielens_data[i,2],1])
}

#create clusters for movies genres
library(cluster)
library(data.table) # storing data in data.table
library(fpc) # for cluster.stats method used for evaluation of clustering


# choosing the number of clusters
# in such a way that I am trying to minimize the within/between ration
# and because that ratio monotonically decreases, I decide to break when the
# decrease of that ratio is not significant 
# I am using the so called "Ellbow method" for choosing the number of clusters
# The elbow occurs at k = 11, so I am choosing later k = 11 to be the number of clusters
wb = rep(0, 20) # used to store within/between ratios

d = dist(itemsData[,-1], method = 'binary');# because the data which describes the movies is
# binary - belongs or not to a specific genr 1/0

# using average method
clustMovies = hclust(d, method = 'average');# doing the clustering

for(i in c(2:20)) {
  numClusters = i; # trying every value from 2 to 20 for number clusters k
  groups = cutree(clustMovies, k = numClusters);# assignment to a group for every movie
  
  # storing the withing/between ratios
  wb[i] = cluster.stats(d, groups)$wb
}

numClusters = c(2:20) # all numbers of cluster tried
withinBetweenRatio = wb[2:20] # all within/between ratios obtained

#the graphic which shows the decrease of within/between ratio as the number of clusters grows
# there are other options to break the agglomeration, but as long the elbow method is
# more popular for the k-means clustering, it is also applicable for the Hierarchical 
# Agglomerative Clustering
plot(numClusters, withinBetweenRatio)
lines(numClusters, withinBetweenRatio) 
dev.off()
numClusters = 11 # I chose it after analysing the graphic, that's where the elbow occurs

groups = cutree(clustMovies, k = numClusters) # cut the clustering tree, creating 11 clusters
moviesClustered = itemsData;
moviesClustered$clusterId = groups;# appended to moviesData the clusterId

# shows many properties of the clustering, such as between distance, within distance, etc.
stats = cluster.stats(d, groups)



#explore
vector_ratings <- as.vector(movies_rating@data)


table(vector_ratings)

vector_ratings <- vector_ratings[vector_ratings != 0]

vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + ggtitle("Distribution of the ratings")

#Content base filtering
library(reshape2)

genre_matrix = as.data.frame(itemsData[-1])


#Now, what we need is a user profile matrix. 
#This can be easily done with the dcast() function in the reshape2 package. 
#I first convert the ratings into a binary format to keep things simple. ratings of 4 and 5 are mapped to 1, representing likes, 
#and ratings of 3 and below are mapped to -1, representing dislikes.

binaryratings = movielens_data
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}


#To obtain the binaryratings matrix in the correct format we need, I use the dcast() function in the reshape2 package. 
#This basically transforms the data from a long format to a wide format. 
#This also creates many NA values because not every user rated every movie. I substituted the NA values with 0.

binaryratings2 <- dcast(binaryratings, item.id~user.id, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}

#In this matrix rows represent the movieIds, and  cols, representing the userIds.
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

#To create the simple user profile matrix, 
#I calculated the dot product of the movie genre matrix and the binaryratings matrix.
#Calculate dot product for User Profiles
result = matrix(0,18,943)

for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix)){
    result[i,c] <- sum((genre_matrix[,i]) * (binaryratings2[,c]))
  }
}


#Convert to Binary scale
for (i in 1:length(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

#This user profiles shows the aggregated inclination of each user towards movie genres. 
#Each column represents a unique userId, and positive values shows a preference towards a certain genre. 
#The values were again simplified into a binary matrix — positive values were mapped to 1 to represent likes, 
#negative values were mapped to 0 to represent dislikes.

#Assume that users like similar items, and retrieve movies that are closest in similarity to a user’s profile, 
#which represents a user’s preference for an item’s feature.


result2=result[,1]
sim_mat <- rbind.data.frame(result2, genre_matrix)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)}))#convert data to type integer

suggest_movies=rep(list(NA),943)
all_movies_dist = rep(list(NA),943)

for ( i in 1:943) {
  result2=result[,i]
  sim_mat = rbind.data.frame(result2,genre_matrix)
  sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)}))



#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:1682]))
rows <- which(sim_results == min(sim_results))
all_movies_dist[[i]] = which(order(sim_results[1:1682,])<=100)
suggest_movies[[i]]=rows
#Recommended movies

}
itemsData[rows,]

#Evaluate content based method

ROC_10 = rep(list(NA),943)

for (i in 1:943) {
  TP = sum(all_movies_dist[[i]][1:10]%in%id_mov[[i]])
  TPR = TP/length(id_mov[[i]])
  FP  = length(all_movies_dist[[i]][1:10])-TP
  FPR = FP / (1682-length(id_mov[[i]]))
  ROC_10[[i]] = c(TPR,FPR)
}


for (i in 1:943) {
  TPR_mean_100 = mean(ROC_100[[i]][1])
}
TPR_total = c(TPR_mean_10,TPR_mean_20,TPR_mean_30,TPR_mean_40,TPR_mean_50,TPR_mean_60,TPR_mean_70,TPR_mean_80,TPR_mean_90,TPR_mean_100)

for (i in 1:943) {
  FPR_mean_100 = mean(ROC_100[[i]][2])
}
FPR_total = c(FPR_mean_10,FPR_mean_20,FPR_mean_30,FPR_mean_40,FPR_mean_50,FPR_mean_60,FPR_mean_70,FPR_mean_80,FPR_mean_90,FPR_mean_100)

table_plot = cbind(FPR_total,TPR_total)
rownames(table_plot)=list("10","20","30","40","50","60","70","80","90","100")

plot(table_plot,type="b",col="red", xlab = "FPR",ylab = "TPR")
text(table_plot,labels=rownames(table_plot),pos=3)
title("ROC Curve")


#collaborative filtering


##split-method
min(rowCounts(movies_rating))


eval_sets <- evaluationScheme(data = movies_rating, method =
                                "split", train = 0.8, given = 18,
                              goodRating = 4, k = 10)

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method =
                                                "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method =
                                                "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method =
                                                "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method =
                                                "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")

##boostrap##
min(rowCounts(movies_rating))


eval_sets <- evaluationScheme(data = movies_rating, method =
                                "bootstrap", train = 0.8, given = 18,
                              goodRating = 4, k = 10)

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method =
                                                "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method =
                                                "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method =
                                                "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method =
                                                "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")


#cross-validation

min(rowCounts(movies_rating))


eval_sets <- evaluationScheme(data = movies_rating, method =
                                "cross-validation", train = 0.8, given = 18,
                              goodRating = 4, k = 10)

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method =
                                                "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method =
                                                "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method =
                                                "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method =
                                                "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n
                         = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft") 
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")