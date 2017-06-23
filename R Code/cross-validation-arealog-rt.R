# This script is used to perform 10 fold cross validation with 30 repeats for the linear models

set.seed(12)
k <-10  #the number of folds
R <- 30 #the number of repeats

folds <- cvFolds(nrow(fdata_rt), K=k, R=R)


# Here I am creating a dataframe to save the predictions of each repetition of the 10-fold CV
# I am also putting in this dataframe the log transformed area so we can calculate the MAD and RMSE
pred_matrix <- matrix(rep(0,R*nrow(fdata_rt)),nrow(fdata_rt),R, byrow=T)
predictions <- data.frame(pred_matrix)
predictions$arealog <- fdata_rt$arealog



for(j in 1:R)
{
  for(i in 1:k)
  {
    train <- fdata_rt[folds$subsets[folds$which != i,j], ] #Set the training set
    validation <- fdata_rt[folds$subsets[folds$which == i,j], ] #Set the validation set
  
#   newlm <- rpart(as.formula(paste("arealog ~", sel_var)), method = "anova", 
#                                    data = train, control = rpart.control(minbucket = 26))
    if (cp_value == 0) { 
          newlm <- rpart(as.formula(paste("arealog ~", sel_var)), method = "anova", 
          #   data = train, control = rpart.control(minbucket = nofbuckets, (as.character.numeric_version(paste("cp =", cp_value)))))
          data = train, control = rpart.control(minbucket = nofbuckets))
        
    } else    {  
                    newlm <- rpart(as.formula(paste("arealog ~", sel_var)), method = "anova", 
                    data = train, control = rpart.control(minbucket = nofbuckets), cp = cp_value)
    }
    newpred <- predict(newlm,newdata=validation) #Get the predicitons for the validation set (from the model just fit on the train data)
    predictions[folds$subsets[folds$which == i,j], j] <- newpred  #Put the hold out prediction in the data set for later use
  }
}

sum_abs=0
sum_squares=0

for(m in 1:R)
{
  sum_abs=sum_abs+(abs((2.718268237^predictions[,R+1]-1)-(2.718268237^predictions[,m]-1)))
  sum_squares=sum_squares+(((2.718268237^predictions[,R+1]-1)-(2.718268237^predictions[,m]-1))^2)
}

RMSE = sqrt(mean(sum_squares/R))
MAD = mean(sum_abs/R)

RMSE
MAD


