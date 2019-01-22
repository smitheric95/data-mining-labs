library(caret)

library(doParallel)

ds_train <- read.csv('ds_train_binary.csv')
data_test <- read.csv('data_test_binary.csv')

# ds_folds - indices for 10-fold CV of each fold
load('ds_folds_binary')

# pass index=ds_folds, to use ds_folds as indices for 10-fold CV
sens <- function(data, lev=NULL, model=NULL) {
    sens_val <- sensitivity(data$pred, data$obs, positive='1+ days')
    c(Sens=sens_val)
}

control <- trainControl(method='cv', index=ds_folds, summaryFunction=sens)

#registerDoParallel()

# example - just make sure you pass `control` when training other models
#           to use the same folds
grid  <- expand.grid(mtry=c(2))
rf <- train(days_in_hospital ~ .,
            data=ds_train, method='rf', tuneLength=1,
            metric='Sens',
            ntree=2, trControl=control)

p <- predict(rf, data_test)
confusionMatrix(p, data_test$days_in_hospital, positive="1+ days")
