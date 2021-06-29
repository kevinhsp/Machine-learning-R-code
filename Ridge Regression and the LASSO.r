########################################################
############ Ridge Regression and the LASSO ############
########################################################

##################################
############# R code #############
##################################

library(ISLR)    # to access the Hitters dataset
library(dplyr)   # to access the select() function
library(glmnet)  # to access the cv.glmnet() function


# let's examine the Hitters dataset
str(Hitters)
View(Hitters)


# remove any variables you don't want included; e.g., if you wanted to exclude
#  CHits, you would run: select(Hitters, -CHits)
# we want to include all of the variables here, though


# after selecting the variables you'll include, remove any rows w/ NAs
Hitters_noNA <- na.omit(Hitters)


# the cv.glmnet() requires the response and predictors to (1) be in separate
#  objects and (2) be in matrices, not data frames
all_y <- Hitters_noNA$Salary

# use model.matrix() for the x data; it also converts qualitative variables into 
#  dummy variables -- good
# always include the [, -1] at the end
all_x <- model.matrix(Salary ~ ., data = Hitters_noNA)[, -1]


# randomly sample 1/4 of the observations for the test set
set.seed(100)
indices <- sample(1:nrow(Hitters_noNA), 1/4*nrow(Hitters_noNA), replace = FALSE)


# we also need to separate the training and test sets into different objects
#  this just involves subsetting matrices
test_x <- all_x[indices, ]
test_y <- all_y[indices]  # subsetting a vector, so no need to specify columns
training_x <- all_x[-indices, ]
training_y <- all_y[-indices]



# use cv.glmnet() to find the optimal lambda for ridge regression and lasso
# alpha = 0 --> ridge regression; alpha = 1 --> lasso
# nfolds = number of CV folds --> default = 10

##### Ridge regression -- find optimal lambda
################################################
set.seed(100)

# cv.glmnet() w/ alpha = 0 --> ridge regression
cv_ridge <- cv.glmnet(x = training_x, y = training_y, alpha = 0)


# plot of training MSE based on log(lambda)
plot(cv_ridge)  # you don't need to run this


# subset the optimal lambda for ridge regression
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_ridge


##### LASSO -- find optimal lambda
################################################
set.seed(100)

# cv.glmnet() w/ alpha = 1 --> lasso
cv_lasso <- cv.glmnet(x = training_x, y = training_y, alpha = 1)


# plot of training MSE based on log(lambda)
plot(cv_lasso)  # you don't need to run this


# subset the optimal lambda for lasso
best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_lasso



##### Ridge regression -- find RMSE
################################################

pred_ridge <- predict(object = cv_ridge, s = best_lambda_ridge, newx = test_x)
rmse_ridge <- sqrt(mean((pred_ridge - test_y)^2))  # salaries are in $1,000s
rmse_ridge



##### LASSO -- find RMSE
################################################

pred_lasso <- predict(object = cv_lasso, s = best_lambda_lasso, newx = test_x)
rmse_lasso <- sqrt(mean((pred_lasso - test_y)^2))  # salaries are in $1,000s
rmse_lasso



##### Ordinary least squares (OLS) regression -- find RMSE
########################################################

# remember that least squares regression = ridge regression w/ lambda = 0
pred_OLS <- predict(object = cv_ridge, s = 0, newx = test_x)
rmse_OLS <- sqrt(mean((pred_OLS - test_y)^2))  # salaries are in $1,000s
rmse_OLS




##### compare results of ordinary least squares regression, 
##### ridge regression, and LASSO
#####################################################################

# for whichever model you decide on, refit the model using the ENTIRE dataset
# if using ridge regression or LASSO, use the optimal lambda found earlier

# use glmnet() to fit the final model and coef() to find the coefficients

# find final ridge coefficients
glmnet_ridge <- glmnet(x = all_x, y = all_y, alpha = 0, lambda = best_lambda_ridge)
ridge_coeffs <- coef(glmnet_ridge)


# find final LASSO coefficients
glmnet_lasso <- glmnet(x = all_x, y = all_y, alpha = 1, lambda = best_lambda_lasso)
lasso_coeffs <- coef(glmnet_lasso)


# find final OLS coefficients
lm_info <- lm(all_y ~ all_x)
ols_coeffs <- lm_info$coefficients
ols_coeffs


# combine all 3 sets of final coefficients in the same data frame -- you don't
#  need to do this
data.frame(
  Least.Squares = ols_coeffs, 
  Ridge = as.vector(ridge_coeffs), 
  LASSO = as.vector(lasso_coeffs)
)
