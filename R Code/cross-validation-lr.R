#Cross Validation - Logistic Regression

set.seed(1)
k <-10 #the number of folds
R <-30
folds <- cvFolds(nrow(fdata_lr), K=k, R=R)
sum<-0
for(j in 1:R)
{
  for(i in 1:k)
  {
    train <- fdata_lr[folds$subsets[folds$which != i,j], ] #Set the training set
    validation <- fdata_lr[folds$subsets[folds$which == i,j], ] #Set the validation set

    newlm <- glm(as.formula(paste("classarea ~", sel_var_lr)), family = "binomial", data=train, maxit=100)
               
    p <- predict(newlm)
    pr<-prediction(p, train$classarea)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    sum<-sum+auc
}
}
auc_ave<-sum/(R*k)
auc_ave