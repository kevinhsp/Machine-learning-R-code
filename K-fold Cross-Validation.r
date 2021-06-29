##### How to manually run k-fold cross-validation using a for loop
#####################################################################

# load the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


##### initial stuff
#################################################

# set a seed for reproducibility since k-fold CV involves random numbers
set.seed(100)

# initial stuff
k <- 5    # number of folds
rmse <- vector()

# store the regression coefficients for each run; each row = new run
# 4 = number of parameters in the regression model (including the intercept)
coeffs_mat <- matrix(nrow = k, ncol = 4)



##### randomly assign observations to k folds
#################################################

# randomly permute the observations/rows
perm_rows <- sample(nrow(colleges), nrow(colleges), replace = FALSE)

# divide data into K equally-sized groups (the folds)
folds <- split(perm_rows, as.factor(1:K))  # data structure = a list here



##### for loop -- each iteration uses different 
##### folds for training and test data
#################################################

for(i in 1:k){
  
  # split full dataset into training and test sets
  test <- colleges[folds[[i]], ]
  training <- colleges[-folds[[i]], ]
  
  # fit regression model to training set
  lm_info <- lm(gradrate ~ math + facratio + public, data = training)
  
  # store coefficients
  coeffs_mat[i, ] <- lm_info$coefficients
  
  # residual = actual grad rate (from test set!) - predicted grad rate
  resids <- test$gradrate - predict(object = lm_info, newdata = test)

  # calculate RMSE for each run
  rmse[i] <- sqrt(mean(resids^2))
}



##### results
#################################################

# print regression coefficients for each run
coeffs_mat


# print RMSEs for each run
rmse


# find overall RMSE
sqrt(mean(rmse^2))


# fit the final model to full dataset if you're happy w/ the CV results (and
# thus that model)
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)
lm_info$coefficients  # or summary(lm_info)