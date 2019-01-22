setwd("~/Desktop/Data Mining/data-mining-labs/Project3")
##################################################################################################################
install.packages('ModelMetrics', dependencies = TRUE)

library(RWeka)
library(ModelMetrics)
library(randomForest)
library(createFolds)
library(train)

library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)
library(doParallel)
library(keras)
library(pROC)
library(lattice)
##################################################################################################################
members <- read.csv(file="./Data/flatMembersY1.csv")
summary(members$DaysInHospital)

##################################################################################################################
# import train/test splits

ds_train <- read.csv('./Data/ds_train.csv')
ds_test <- read.csv('./Data/ds_test.csv')

ds_train_X <- normalize(as.matrix(ds_train[,-ncol(ds_train)]))
ds_train_y <- to_categorical(as.integer(ds_train[,ncol(ds_train)]))

ds_test_X <- normalize(as.matrix(ds_test[,-ncol(ds_test)]))
ds_test_y <- to_categorical(as.integer(ds_test[,ncol(ds_test)]))

# ds_folds - indices for 10-fold CV of each fold
load('./Data/ds_folds')

# pass index=ds_folds, to use ds_folds as indices for 10-fold CV
control <- trainControl(method='cv', index=ds_folds)
registerDoParallel()

# example - just make sure you pass `control` when training other models
#           to use the same folds
rf <- train(days_in_hospital ~ .,
						data=ds_train, method='rf', tuneLength=1,
						ntree=2, trControl=control)
rf
##################################################################################################################
# Keras first model
# install_keras()
model <- keras_model_sequential()

model %>%
	layer_dense(units = ncol(ds_train_X), activation = 'relu', input_shape = c(ncol(ds_train_X))) %>%
	# layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(ds_train_X))) %>%
	# layer_dense(units = 8, activation = 'relu', input_shape = c(ncol(ds_train_X))) %>%
	layer_dense(units = ncol(ds_train_y), activation = 'softmax')
model

model %>% compile(
	loss = 'categorical_crossentropy',
	optimizer = 'adam',
	metrics = 'accuracy'
)

history <- model %>% fit(
	ds_train_X,
	ds_train_y,
	epochs = 200,
	control=control,
	batch_size = 5
)

classes <- model %>% predict_classes(ds_test_X, batch_size = 128)

confusionMatrix(data = factor(classes, levels = 1:length(levels(ds_train$days_in_hospital)), labels = levels(ds_train$days_in_hospital)),
								ref = ds_test[,ncol(ds_test)])


# folds <- createFolds(y = raw_data[,target], k = 5, list = F)

##################################################################################################################
ds_train <- read.csv('./Data/ds_train_binary.csv')
ds_test <- read.csv('./Data/data_test_binary.csv')

ds_train_X <- normalize(as.matrix(ds_train[,-ncol(ds_train)]))
ds_train_y <- as.integer(ds_train[,ncol(ds_train)]) - 1

ds_test_X <- normalize(as.matrix(ds_test[,-ncol(ds_test)]))
ds_test_y <- as.integer(ds_test[,ncol(ds_test)]) - 1

# ds_folds - indices for 10-fold CV of each fold
load('./Data/ds_folds_binary')

# pass index=ds_folds, to use ds_folds as indices for 10-fold CV
control <- trainControl(method='cv', index=ds_folds)

registerDoParallel()

# example - just make sure you pass `control` when training other models
#           to use the same folds
grid  <- expand.grid(mtry=c(7))
rf <- train(days_in_hospital ~ .,
						data=ds_train, method='rf', tuneLength=1,
						ntree=50, trControl=control, tuneGrid=grid)

p <- predict(rf, data_test)
confusionMatrix(p, data_test$days_in_hospital, positive="1+ days")
##################################################################################################################
# TODO: keras model w k-fold

##################################################################################################################
##################################################################################################################
sensitivities = c()

for (fold in ds_folds) {
	train_X = ds_train_X[fold,]
	train_y = ds_train_y[fold,]
	
	test_X = ds_test_X[-fold,]
	test_y = ds_test_y[-fold,]
	
	model <- keras_model_sequential()
	
	model %>%
		layer_dense(units = ncol(train_X), activation = 'relu', input_shape = c(ncol(train_X))) %>%
		layer_dropout(rate = 0.2) %>%
		layer_dense(units = 32, activation = 'relu', input_shape = c(ncol(ds_train_X))) %>%
		layer_dropout(rate = 0.2) %>%
		layer_dense(units = ncol(train_y), activation = 'sigmoid')
	model
	
	model %>% compile(
		loss = 'categorical_crossentropy',
		optimizer = 'adam',
		metrics = 'accuracy'
	)
	
	history <- model %>% fit(
		train_X,
		train_y,
		epochs = 200,
		batch_size = 5
	)
	
	classes <- model %>% predict_classes(test_X, batch_size = 128)

	
	print(sensitivity(data = factor(classes, levels = 0:1, labels = levels(ds_train$days_in_hospital)),
																	 ref = ds_test[-fold,ncol(ds_test)], positive="1+ days"))
}

print(mean(sensitivities))
##################################################################################################################
# ROC!

r <- roc(test_y[,1] == 1, classes)
png('./plots/neural-roc.png', width=4, height=3.25, units='in', res=1200, pointsize=4)
r <- roc(test_y[,1] == 1, classes)
par(cex.lab=3, cex.axis=2, cex.main=1, cex.sub=1.5)
plot(r, mar=c(7,7,4,7)+0.1, mgp=c(5, 1, 0))
dev.off()
##################################################################################################################
# heatmap
c_svmr <- confusionMatrix(data = factor(classes, levels = 0:1, labels = levels(ds_train$days_in_hospital)),
													ref = ds_test[-fold,ncol(ds_test)], positive="1+ days")

svm_tab <- prop.table(c_svmr$table, 2)

myPanel <- function(x, y, z, ...) {
	panel.levelplot(x,y,z,...)
	panel.text(x, y, round(z,4))
}

png('./plots/neural-heat.png', width=3.75, height=3.25, units='in', res=1200, pointsize=4)
levelplot(svm_tab, panel=myPanel, at=(0:100/100),
					aspect='fill', col.regions=rev(gray(0:100/100)))
dev.off()
##################################################################################################################
num_trees <- seq(5,525,by=200)
sensitivities = c()

for (n in num_trees) {
	grid  <- expand.grid(mtry=c(7))
	rf <- train(days_in_hospital ~ .,
							data=ds_train, method='rf', tuneLength=1,
							ntree=n, trControl=control, tuneGrid=grid)
	
	p <- predict(rf, ds_test)
	sensitivities <- c(sensitivities, sensitivity(p, ds_test$days_in_hospital, positive="1+ days"))
}

png("./plots/trees-line.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 8)
plot(num_trees, sensitivities, xlab="Number of Trees", ylab="Recall")
lines(num_trees, sensitivities)
dev.off()

c_svmr <- confusionMatrix(p, ds_test$days_in_hospital, positive="1+ days")

svm_tab <- prop.table(c_svmr$table, 2)

myPanel <- function(x, y, z, ...) {
	panel.levelplot(x,y,z,...)
	panel.text(x, y, round(z,4))
}

png('./plots/trees-heat.png', width=3.75, height=3.25, units='in', res=1200, pointsize=4)
levelplot(svm_tab, panel=myPanel, at=(0:100/100),
					aspect='fill', col.regions=rev(gray(0:100/100)))
dev.off()

r <- roc(ds_test$days_in_hospital == '1+ days', as.numeric(p) - 1)
png('./plots/trees-roc.png', width=4, height=3.25, units='in', res=1200, pointsize=4)
r <- roc(ds_test$days_in_hospital == '1+ days', as.numeric(p) - 1)
par(cex.lab=3, cex.axis=2, cex.main=1, cex.sub=1.5)
plot(r, mar=c(7,7,4,7)+0.1, mgp=c(5, 1, 0))
dev.off()
#############