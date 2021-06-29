########################################################
#################### Holdout Method ####################
########################################################

##################################
############ R code 1 ############
##################################

# load the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


##### setting a seed

# set a seed for reproducible results whenever generating random numbers
set.seed(1)  # nothing special about the number 1


# sample() draws random samples -- example unrelated to colleges
sample(1:10, 5, replace = FALSE)  # randomly draw 5 #'s between 1 and 10 w/o rep

sample(1:10, 5, replace = FALSE)  # new set of numbers!


# reset seed at 1
set.seed(1)
sample(1:10, 5, replace = FALSE)  # same as when ran lines 19 then 23 above


# new seed --> different random numbers
set.seed(1000) 
sample(1:10, 5, replace = FALSE)



# I'm resetting the seed -- now you can practice w/ the code above w/o worrying 
# about potentially getting results different from mine
set.seed(1)


# use sample() to randomly sample 1/4 of the 120 observation/row numbers
indices <- sample(nrow(colleges), 1/4*120, replace = FALSE)
indices


# the test set gets the 30 (120/4) observations w/ these row numbers
test <- colleges[indices, ]


# the training set gets the remaining 90 observations
training <- colleges[-indices, ]


# fit model to the training data only
lm_info <- lm(gradrate ~ math + facratio + public, data = training)

# print the coefficients
lm_info$coefficients  # or use summary(lm_info) -- your choice


# make predictions using fitted model and test data
pred <- predict(object = lm_info, newdata = test)


# calculate RMSE
rmse <- sqrt(mean((test$gradrate - pred)^2))
rmse




#######################################################################
########## k-Fold Cross-Validation for Regression -- 1 Model ##########
#######################################################################

##################################
############ R code 2 ############
##################################

# load the caret package to access the train() function
library(caret)


# load the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


# set a seed for reproducibility of k-fold CV results since the method involves
# random numbers
set.seed(100)


# use train() w/ specific inputs below to run CV
cv_info <- train(gradrate ~ math + facratio + public, data = colleges, 
  method = "lm",    # use "lm" if you want to run linear regression
  trControl = trainControl(method = "cv", number = 5)    # number = # of folds
)


# now the train() output contains some really useful info
cv_info


# fit the final model to ALL the data if you're happy w/ the CV results (and
#   thus that model)
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)


# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
summary(lm_info)




###################################################################
############ Leave-One-Out Cross-Validation -- 1 Model ############
###################################################################

##################################
############ R code 3 ############
##################################

# set a seed
set.seed(100)


# use train() w/ method = "LOOCV" to run LOOCV
loocv_info <- train(gradrate ~ math + facratio + public, data = colleges, 
  method = "lm",    # use "lm" if you want to run linear regression
  trControl = trainControl(method = "LOOCV")
)


# look at train() output for RMSE
loocv_info


# fit the final model to ALL the data if you're happy w/ the CV results (and
#   thus that model) -- same code as for k-fold CV
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)


# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
summary(lm_info)




#######################################################################
######### k-Fold Cross-Validation for Regression -- 2+ Models #########
#######################################################################


##################################
############ R code 4 ############
##################################

# make sure you set a seed before you use train() for EACH model or method!!
# guarantees same training and test sets used for each model or method

# in train(), na.action = na.omit omits any rows w/ missing values (NAs) from 
#    the analysis
# there are other options you can supply to na.action, but they involve imputing 
#   missing data, which is beyond the scope of this course

# model 1: Budget (B)
set.seed(100)
cv_info_B <- train(WorldGross ~ Budget, data = movies, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5),
  na.action = na.omit
)


# model 2: Budget (B) and RottenTomatoes (R)
set.seed(100)
cv_info_BR <- train(WorldGross ~ Budget + RottenTomatoes, data = movies, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5),
  na.action = na.omit
)


# model 3: budget (B) and AudienceScore (A)
set.seed(100)
cv_info_BA <- train(WorldGross ~ Budget + AudienceScore, data = movies, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5),
  na.action = na.omit
)


# compare RMSEs -- train() output variable name followed by $results$RMSE
cv_info_B$results$RMSE
cv_info_BR$results$RMSE
cv_info_BA$results$RMSE  # best model of the 3 since has lowest RMSE


# fit the best of the fitted models to ALL of the data
lm_info <- lm(WorldGross ~ Budget + AudienceScore, data = movies)


# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
summary(lm_info)