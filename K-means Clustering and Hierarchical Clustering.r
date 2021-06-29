########################################################
################### k-Means Clustering #################
########################################################

##################################
############ R code 1 ############
##################################

library(tidyverse)   # to access select() and others (see last part of code)
library(factoextra)  # to access the fviz_cluster() and fviz_nbclust() functions
library(gridExtra)   # to access the grid.arrange() function

drivers <- read.table("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/delivery_drivers.txt",
  header = TRUE
)


# remove any variables you don't want
drivers2 <- select(drivers, -Driver_ID)


# remove rows w/ missing data
drivers_noNA <- na.omit(drivers2)


# use scale() to standardize the data
drivers_scaled <- scale(drivers_noNA)


# use fviz_nbclust() to create a plot that helps find the optimal number of 
#  clusters; method = "wss" --> elbow method (what we'll use); 
#  method = "silhouette" --> average silhouette method
set.seed(1)  # first set a seed

# make sure to use the SCALED data here (and below)!!
fviz_nbclust(x = drivers_scaled, FUNcluster = kmeans, method = "wss")


# use kmeans() to run k-means; centers = number of clusters; nstart = number of 
#  initial configurations of the data (25 is safe) --> kmeans() then uses the 
#  best initial configuration of those tried
set.seed(1)
k4 <- kmeans(x = drivers_scaled, centers = 4, nstart = 25)
k4


# use fviz_cluster() to plot the clusters; if there are more than 2 variables, 
#  the function will plot the most important principal components -- things that
#  are beyond the scope of this course, but know they are NOT original variables
fviz_cluster(object = k4, data = drivers_scaled, geom = "point")


### use multiple dplyr functions to calculate summary statistics for each cluster
#   first must make sure the dataset is a data frame!

# k = 4 results
drivers_noNA %>%  # use the dataset w/ UNstandardized data
  mutate(Cluster = k4$cluster) %>%  # mutate() creates a new variable
  group_by(Cluster) %>%         # group_by() creates groups based on a variable
  summarise_all(c("mean", "sd"))  # summarise_all() calculates summary stats



# you can run k-means using different values of k (we already used k = 2 above)
set.seed(1)  # if you rerun any of these, make sure to reset this seed
k2 <- kmeans(x = drivers_scaled, centers = 2, nstart = 25)
k3 <- kmeans(x = drivers_scaled, centers = 3, nstart = 25)
k5 <- kmeans(x = drivers_scaled, centers = 5, nstart = 25)
k6 <- kmeans(x = drivers_scaled, centers = 6, nstart = 25)


### create plots of the clusters, but only store them for now
# k = 2 results
plot_k2 <- fviz_cluster(k2, geom = "point", data = drivers_scaled) + ggtitle("k = 2")

# k = 3 results
plot_k3 <- fviz_cluster(k3, geom = "point", data = drivers_scaled) + ggtitle("k = 3")

# k = 4 results
plot_k4 <- fviz_cluster(k4, geom = "point", data = drivers_scaled) + ggtitle("k = 4")

# k = 5 results
plot_k5 <- fviz_cluster(k5, geom = "point", data = drivers_scaled) + ggtitle("k = 5")

# k = 6 results
plot_k6 <- fviz_cluster(k6, geom = "point", data = drivers_scaled) + ggtitle("k = 6")


# use grid.arrange() to display ggplot2-based plots side by side
grid.arrange(plot_k2, plot_k3, plot_k4, plot_k5, plot_k6)


#### results for other values of k
####################################

### k = 2 results
# plot
plot_k2

# summaries
drivers_noNA %>%  # use the dataset w/ UNstandardized data
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(c("mean", "sd"))


### k = 3 results
# plot
plot_k3

# summaries
drivers_noNA %>%  # use the dataset w/ UNstandardized data
  mutate(Cluster = k3$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(c("mean", "sd"))




##################################
############ R code 2 ############
##################################

library(tidyverse)   # to access select() and others (see last part of code)
library(factoextra)  # to access the fviz_cluster() and fviz_nbclust() functions
library(gridExtra)   # to access the grid.arrange() function

colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/US_Colleges.csv",
  header = TRUE
)


# run this if you later want to determine which colleges are in which cluster
colleges_with_names <- select(colleges, name, verbal, math, phds, gradrate)
colleges_with_names2 <- na.omit(colleges_with_names)


# select only the variables you want
colleges2 <- select(colleges, verbal, math, phds, gradrate)


# remove rows w/ missing data
colleges_noNA <- na.omit(colleges2)


# use scale() to standardize the data
colleges_scaled <- scale(colleges_noNA)


# use fviz_nbclust() to create a plot that helps find the optimal number of 
#  clusters; method = "wss" --> elbow method (what we'll use); 
#  method = "silhouette" --> average silhouette method
set.seed(1)  # first set a seed

# make sure to use the SCALED data here (and below)!!
fviz_nbclust(x = colleges_scaled, FUNcluster = kmeans, method = "wss")


# use kmeans() to run k-means; centers = number of clusters; nstart = number of 
#  initial configurations of the data (25 is safe) --> kmeans() then uses the 
#  best initial configuration of those tried
set.seed(1)  # first set a seed
k3_colleges <- kmeans(x = colleges_scaled, centers = 3, nstart = 25)
k3_colleges


# use fviz_cluster() to plot the clusters; if there are more than 2 variables, 
#  the function will plot the most important principal components -- things that
#  are beyond the scope of this course, but know they are NOT original variables
fviz_cluster(object = k3_colleges, data = colleges_scaled, geom = "point")


# use multiple dplyr functions to calculate summary statistics for each cluster
# first must make sure the dataset is a data frame!
colleges_noNA %>%  # use the dataset w/ UNstandardized data
  mutate(cluster = k3_colleges$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean)


# add variable to dataset that indicates the cluster each observation belongs to
colleges_3_cluster <- colleges_with_names2 %>% 
  mutate(cluster = k3_colleges$cluster) %>%
  arrange(cluster)

View(colleges_3_cluster)



# you can run k-means using different values of k (we already used k = 2 above)
set.seed(1)  # if you rerun any of these, make sure to reset this seed
k2_colleges <- kmeans(x = colleges_scaled, centers = 2, nstart = 25)
k4_colleges <- kmeans(x = colleges_scaled, centers = 4, nstart = 25)
k5_colleges <- kmeans(x = colleges_scaled, centers = 5, nstart = 25)
k6_colleges <- kmeans(x = colleges_scaled, centers = 6, nstart = 25)
k7_colleges <- kmeans(x = colleges_scaled, centers = 7, nstart = 25)


### create plots of the clusters, but only store them for now
# k = 2 results
plot_k2_colleges <- fviz_cluster(k2_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 2")

# k = 3 results
plot_k3_colleges <- fviz_cluster(k3_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 3")

# k = 4 results
plot_k4_colleges <- fviz_cluster(k4_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 4")

# k = 5 results
plot_k5_colleges <- fviz_cluster(k5_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 5")

# k = 6 results
plot_k6_colleges <- fviz_cluster(k6_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 6")

# k = 7 results
plot_k7_colleges <- fviz_cluster(k7_colleges, geom = "point", data = colleges_scaled) + 
  ggtitle("k = 7")


# use grid.arrange() to display ggplot2-based plots side by side
grid.arrange(plot_k2_colleges, plot_k3_colleges, plot_k4_colleges, 
  plot_k5_colleges, plot_k6_colleges, plot_k7_colleges
)


#### results for other values of k
####################################

### k = 5 results
# plot
plot_k5_colleges

# summary statistics (means here) for each cluster
colleges_noNA %>%  # use the dataset w/ UNstandardized data
  mutate(cluster = k5_colleges$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean)


# add variable to dataset that indicates the cluster each observation belongs to
colleges_5_cluster <- colleges_with_names2 %>% 
  mutate(cluster = k5_colleges$cluster) %>%
  arrange(cluster)

View(colleges_5_cluster)




########################################################
################# Hierarchical Clustering ##############
########################################################

##################################
############ R code 3 ############
##################################

library(tidyverse)   # to access select() & other fxns (pipe operator & others
library(factoextra)  # to access the fviz_cluster() fxn

states <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/State_Data.csv",
  header = TRUE
)


# select variables of interest
states2 <- select(states, -State, -Pop)


# store variables of interest and state names -- then can later determine which
#  states belong to which cluster (not just their row #'s)
states_with_names <- select(states, -Pop)
states_with_names2 <- na.omit(states_with_names)


# make the states the row names if want to include the state names in plots
row.names(states2) <- states$State


# remove observations w/ NAs
states_noNA <- na.omit(states2)


# standardize data
states_scaled <- scale(states_noNA)


# use dist() w/ method = "euclidean" to calculate pairwise Euclidean distances
dist_mat <- dist(states_scaled, method = "euclidean")


# use hclust() to run a hierarchical clustering algorithm
hc_single <- hclust(dist_mat, method = "single")  # single linkage

hc_complete <- hclust(dist_mat, method = "complete")  # complete linkage

hc_average <- hclust(dist_mat, method = "average")  # average linkage
hc_average  # just to see what the output looks like -- more is actually stored


# make dendrogram using plot() and hclust() output
plot(hc_single, hang = -1)
plot(hc_complete, hang = -1)
plot(hc_average, hang = -1)


# use rect.hclust to display rectangles around clusters; k = # of clusters
# first need to make the dendrogram
plot(hc_average, hang = -1)
rect.hclust(hc_average, k = 2, border = 2:3)  # 2 clusters


# reset plot before trying a different value for k
plot(hc_average, hang = -1)
rect.hclust(hc_average, k = 3, border = 2:4)

plot(hc_average, hang = -1)
rect.hclust(hc_average, k = 4, border = 2:5)

plot(hc_average, hang = -1)
rect.hclust(hc_average, k = 5, border = 2:6)


# use cutree() to cut the tree so that k clusters result
clusters5 <- cutree(as.hclust(hc_average), k = 5)
clusters5  # somewhat helpful -- better if we rearrange


# determine which states belong to which cluster (in order)
states_5_clusters <- states_with_names %>% 
  mutate(cluster = clusters5) %>%      # using 5 clusters
  arrange(cluster)
View(states_5_clusters)


# calculate summary statistics
states_noNA %>%    # use the dataset w/ UNstandardized data
  mutate(cluster = clusters5) %>%    # using 5 clusters
  group_by(cluster) %>%
  summarise_all(c("mean", "sd"))     # can specify other summaries as well


# use fviz_cluster() to plot the clusters in 2 dimensions
# use repel = TRUE to ensure observations labels aren't plotted on top of each
#  other
fviz_cluster(list(data = states_noNA, cluster = clusters5), repel = TRUE) +
  theme(
    axis.title = element_text(size = 24), axis.text = element_text(size = 24),
    plot.title = element_text(hjust = 0.5, size = 28)
  )