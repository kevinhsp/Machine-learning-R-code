#############################################################
############ Regression and Classification Trees ############
#############################################################

####################################
############# R code 1 #############
####################################

# install the rpart.plot package: install.packages("rpart.plot")

library(dplyr)       # to access the select() function
library(caret)       # to access the train() function
library(rpart.plot)  # to access the prp() function for plotting reg. trees

homes <- read.csv("Desktop/STAT240/Home_Sales.csv", 
  header = TRUE
)

# select only variables of interest -- all except ZIP code
homes2 <- select(homes, -ZIP)


# transform variables that aren't factors into factors -- not always necessary
#  but safest in case have qualitative variables w/ more than 2 evels
str(homes2)
homes2$AIR <- as.factor(homes2$AIR)
homes2$FIREPLACE <- as.factor(homes2$FIREPLACE)


# remove observations (rows) w/ missing values
homes_noNA <- na.omit(homes2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(homes_noNA), 0.25*nrow(homes_noNA), replace = FALSE)
test <- homes_noNA[indices, ]
training <- homes_noNA[-indices, ]


# build regression trees
set.seed(1)
tree_fit <- train(PRICE ~ ., data = training,    # training data!!
  method = "rpart",    # use rpart for regression trees
  trControl = trainControl(method = "cv", number = 5),
  
  # split criterion: "information" for information gain; "gini" for gini index
  parms = list(split = "information"),
  tuneLength = 10  # number of alphas (cp's) to try -- stick w/ 10
)

tree_fit


# store optimal alpha in case you want to access it later
optimal_alpha <- tree_fit$bestTune


# use prp() to plot a regression tree -- notice it's using the final model
prp(tree_fit$finalModel, box.palette = "Blues")


# make predictions using the final regression tree and test data
tree_pred <- predict(object = tree_fit, newdata = test)


# calculate RMSE
sqrt(mean((test$PRICE - tree_pred)^2))


# fit final regression tree using entire dataset and optimal alpha (cp)
tree_fit_final <- train(PRICE ~ ., data = homes_noNA,    # full dataset
  method = "rpart",
  trControl = trainControl(method = "none"),  # don't use CV here
  parms = list(split = "information"),
  tuneGrid = optimal_alpha  # optimal alpha (cp) from earlier
)


# plot final regression tree -- using ALL data and optimal alpha
prp(tree_fit_final$finalModel, box.palette = "Blues")




####################################
############# R code 2 #############
####################################

library(dplyr)       # to access the select() function
library(caret)       # to access the train() function
library(rpart.plot)  # to access the prp() function for plotting reg. trees

books <- read.csv("Desktop/STAT240/AmazonBooks.csv", 
  header = TRUE
)


# select variables of interest only
View(books)
books2 <- select(books, -c(X, Title, Author, Publisher, ISBN.10))


# remember to transform qualitative variables that aren't factors into factors
str(books2)  # no need to transform anything here


# remove observations (rows) w/ missing values
books_noNA <- na.omit(books2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(books_noNA), 0.25*nrow(books_noNA), replace = FALSE)
test <- books_noNA[indices, ]
training <- books_noNA[-indices, ]


# build classification trees
set.seed(1)
tree_fit <- train(AmazonPrice.10 ~ ., data = training,    # training data!!
  method = "rpart",    # use rpart for regression trees
  trControl = trainControl(method = "cv", number = 5),
  
  # split criterion: "information" for information gain; "gini" for gini index
  parms = list(split = "information"),
  tuneLength = 10  # number of alphas (cp's) to try -- stick w/ 10
)

tree_fit


# store optimal alpha in case you want to access it later
optimal_alpha <- tree_fit$bestTune


# use prp() to plot a classification tree
prp(tree_fit$finalModel, box.palette = "Blues", varlen = 0, digits = 10, 
  tweak = 1.5
)


# calculate accuracy
predicted <- predict(object = tree_fit, newdata = test)  # TEST data!
sum(test$AmazonPrice.10 == predicted)/nrow(test)


# build final classification tree using entire dataset and optimal alpha (cp)
tree_fit_final <- train(AmazonPrice.10 ~ ., data = books_noNA,    # full dataset
  method = "rpart",
  trControl = trainControl(method = "none"),  # don't use CV here
  parms = list(split = "information"),
  tuneGrid = optimal_alpha  # optimal alpha (cp) from earlier
)


# plot final classification tree -- using ALL data and optimal alpha
prp(tree_fit_final$finalModel, box.palette = "Blues", varlen = 0, digits = 10, 
  tweak = 1.5
)