########################################################
############## k-Nearest Neighbors (k-NN) ##############
########################################################

##################################
############ R code 1 ############
##################################

# we'll use the caret package for k-NN (and other methods later)
# another common package is called class (just an FYI)


# install the following packages:  caret and e1071
# install.packages(c("caret", "e1071"))

# you only need to load caret and dplyr
library(caret)  # load caret whenever you want to use it


# we'll use the Default dataset from the ISLR package
library(ISLR)

str(Default)


# the default is for binary factors to have levels of 1 and 2, so you can 
#   subtract 1 to end up w/ 0 and 1
# note that you do NOT need to transform the response w/ k-NN
Default$student <- as.numeric(Default$student) - 1

str(Default)  # student now a numeric w/ 0's and 1's = good


# use train(), from caret, w/ method = "knn" to run the k-nn algorithm
# make sure to use the preProcess code below to standardize (= center and scale)
#   your predictor variables!!
# use the trControl code so no resampling is done
# use tuneGrid to set the value of k (the number of nearest neighbors)
knnFit <- train(default ~ student + balance + income, data = Default, 
  method = "knn",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(k = 9),     # k = 9 nearest neighbors
  preProcess = c("center", "scale")
)


# output of train() not too useful at the moment
knnFit


# if you want to include all variables (other than the response), use a . in
# the x variable spot of the formula
knnFit_all <- train(default ~ ., data = Default, 
  method = "knn",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(k = 9),     # k = 9 nearest neighbors
  preProcess = c("center", "scale")
)


# new data -- call it testing (you'll see why that name in a few weeks)
# make sure to use the original variable names
new_data <- data.frame(student = 0, balance = 1000, income = 40000)


# predict the class label of the new data using the output from train() above
predict(object = knnFit, newdata = new_data)




#########################################################
################## Logistic Regression ##################
#########################################################

##################################
############ R code 2 ############
##################################

# load the ISLR package to access the Default dataset
library(ISLR)


# reset the dataset in case you're running this entire file in one session
Default <- ISLR::Default


# note that the default variable is a factor (w/o numbers)
# one way to fix this is to convert to numeric
str(Default)


# convert default to a numeric and remember to subtract 1 to end up w/ 0's & 1's
Default$default <- as.numeric(Default$default) - 1

str(Default)


# use glm() with family = "binomial" for binary logistic regression
# glm = generalized linear model, btw
glm_info <- glm(default ~ balance, data = Default, family = "binomial")


# run summary() on the glm() output to get the model coefficients
summary(glm_info)



##################################
############ R code 3 ############
##################################

# use predict() to make a prediction
# make sure you include type = "response" otherwise you'll end up with the value
#   of logit(p/(1-p)) instead of p
predict(object = glm_info, newdata = data.frame(balance = 1000), type = "response")

predict(object = glm_info, newdata = data.frame(balance = 2000), type = "response")


# can make multiple predictions at once, as you already know
predict(object = glm_info, newdata = data.frame(balance = c(1000, 2000)),
  type = "response"
)



##################################
############ R code 4 ############
##################################

# multiple predictors?  no problem!
# the glm() inputs are very similar to those from lm()
glm_info <- glm(default ~ balance + income + student, data = Default, 
  family = "binomial"
)

summary(glm_info)



##################################
############ R code 5 ############
##################################

# use $coefficients after the glm() output if you want more digits for the
# coefficients
glm_info$coefficients



#################################
############ R code 6 ############
##################################

# load ISLR in case you didn't already this session
library(ISLR)


# reset the dataset in case you're running this entire file in one session
Default <- ISLR::Default


# remember you need to input the original levels of a factor
levels(Default$student)


predict(object = glm_info, 
  newdata = data.frame(balance = 1000, income = 40000, student = "No"), 
  type = "response"
)



#################################
############ R code 7 ############
##################################

# easiest to create a data frame outside of predict() when there are multiple
# new observations
data_new <- data.frame(
  balance = c(1000, 1000, 2000, 1500), 
  income = c(75000, 12000, 62000, 15000),
  student = c("No", "Yes", "No", "Yes")
)

predict(object = glm_info, newdata = data_new, type = "response")




##########################################################
############## Linear Discriminant Analysis ##############
##########################################################

##################################
############ R code 8 ############
##################################

# load the ISLR package to access the Default dataset and caret to access the
# train() function
library(ISLR)
library(caret)


# reset the dataset in case you're running this entire file in one session
Default <- ISLR::Default


# we want the response variable to be a factor -- good
str(Default)


# use the train() function w/ method = "lda" to run LDA using the caret package
# standardize predictors using "center" and "scale" in preProcess -> equal 
#   variance assumption then met
# ALSO make sure to use BoxCox in preProcess -- it transforms each predictor
#   variable so close to normally distributed -> normal assumption then met
# make sure to include the trControl code so no resampling is done
ldaFit <- train(default ~ balance + income, data = Default, 
  method = "lda",
  trControl = trainControl(method = "none"),
  preProcess = c("BoxCox", "center", "scale")
)


# printed output from train() not too useful at the moment
ldaFit


# use predict() to make predict the class of a new observation(s)
predict(object = ldaFit, newdata = data.frame(balance = 2000, income = 40000))




##################################
############ R code 9 ############
##################################

# load the NELS dataset
nels <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/NELS.csv",
  header = TRUE
)

# region is a numeric vector, not a factor
str(nels$region)


# need the response variable (region here) to be a factor
nels$region <- as.factor(nels$region)


# run LDA
ldaFit <- train(region ~ ses + achmat12, data = nels, 
  method = "lda",
  trControl = trainControl(method = "none"), 
  preProcess = c("BoxCox", "center", "scale")
)


# printed output from train() not too useful at the moment
ldaFit


# not necessary to run summary(), but useful here just to get a sense for the
# distribution of each predictor variable
summary(nels[, c("ses", "achmat12")])


# use predict() to make predict the class of a new observation(s)
predict(object = ldaFit, newdata = data.frame(ses = 20, achmat12 = 50))




###################################
############ R code 10 ############
###################################

# data frame w/ data for new observations
data_new <- data.frame(ses = c(15, 24, 32, 5), achmat12 = c(55, 61, 70, 38))

# use predict() to make predictions using same lda output from train() above
predict(object = ldaFit, newdata = data_new)




##########################################################
############ Quadratic Discriminant Analysis #############
##########################################################

##################################
############ R code 11 ############
##################################

# load the ISLR package to access the Default dataset and caret to access the
# train() function
library(ISLR)
library(caret)

# we want the response variable to be a factor -- good
str(Default)


# use the train() function w/ method = "qda" to run QDA using the caret package
# use BoxCox in preProcess -- it transforms each predictor variable so close to 
#   normally distributed -> normal assumption then hopefully reasonably met
# make sure to include the trControl code so no resampling is done
qdaFit <- train(default ~ balance + income, data = Default, 
  method = "qda",
  trControl = trainControl(method = "none"),
  preProcess = c("BoxCox", "center", "scale")
)


# printed output from train() not too useful at the moment
qdaFit


# use predict() to make predict the class of a new observation(s)
predict(object = qdaFit, newdata = data.frame(balance = 2000, income = 40000))



##################################
############ R code 12 ###########
##################################

# load the NELS dataset
nels <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/NELS.csv",
  header = TRUE
)

# region is a numeric vector, not a factor
str(nels$region)


# need the response variable (region here) to be a factor
nels$region <- as.factor(nels$region)


# run QDA
qdaFit <- train(region ~ ses + achmat12, data = nels, 
  method = "qda",
  trControl = trainControl(method = "none"),
  preProcess = c("BoxCox", "center", "scale")
)


# printed output from train() not too useful at the moment
qdaFit


# use predict() to make predict the class of a new observation(s)
predict(object = qdaFit, newdata = data.frame(ses = 20, achmat12 = 50))



###################################
############ R code 13 ############
###################################

# data frame w/ data for new observations
data_new <- data.frame(ses = c(15, 24, 32, 5), achmat12 = c(55, 61, 70, 38))

# use predict() to make predictions using same lda output from train() above
predict(object = qdaFit, newdata = data_new)




##########################################################
###################### Naive Bayes #######################
##########################################################

##################################
############ R code 14 ############
##################################

# load the e1071 package to access the naiveBayes() function
library(e1071)


# load the Phishing dataset
email <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Phishing.csv", 
  header = TRUE
)


# use naiveBayes() to run Naive Bayes
nbFit <- naiveBayes(Phishing ~ PersonalInfo + Link + Typos + Urgency, data = email)

new_data <- data.frame(PersonalInfo = "Yes", Link = "Yes", Typos = "No", 
  Urgency = "Medium"
)


# use predict() to make a prediction using Naive Bayes
predict(object = nbFit, newdata = new_data)


# use predict() w/ type = "raw" to see the probabilities
predict(object = nbFit, newdata = new_data, type = "raw")
