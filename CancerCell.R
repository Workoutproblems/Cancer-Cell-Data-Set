########################################
########  LINEAR REGRESSION  ###########
########################################
# Conduct the linear regression to 
# determine if treat is a significant predictor 
# for survival time and see which other 
# predictors have an effect on survival time.
library(MASS)
attach(VA)
#head(VA)
#str(VA)
#pairs(VA)
#summary(VA)

#  RANDOMLY SPLIT INTO 80/20
#set.seed(1)
#samplesize = 0.80 * nrow(VA)
#index = sample( seq_len( nrow ( VA ) ), size = samplesize)
#datatrain = VA[ index, ]
#datatest = VA[ -index, ]

#  FITTING MODEL
#lm.fit <- lm(stime ~ ., data=VA)
#summary(lm.fit)

#  PREDICT - ERROR
#VAtrain = lm(stime ~ ., data = VA, subset = index)
#mean((VA$stime - predict(VAtrain, VA))[-index]^2)

#  10 TEST ERROR PREDICTIONS
#means <- c(1:10)
set.seed(1)
samplesize = 0.80 * nrow(VA)
meanAvg = 0

for (i in 1:10) {
  
  #  RANDOMLY SPLIT INTO 80/20.
  index = sample( seq_len( nrow ( VA ) ), size = samplesize)
  datatrain = VA[ index, ]
  datatest = VA[ -index, ]
  
  #  FITTING & PREDICTION
  VAtrain = lm(stime ~ ., data = VA, subset = index)
  #means[i] = mean((VA$stime - predict(VAtrain, VA))[-index]^2)
  meanAvg = meanAvg + mean((VA$stime - predict(VAtrain, VA))[-index]^2)
  
}

#  AVG OF 10 LIN. PREDICTIONS
meanAvg = meanAvg / 10
print(meanAvg)

for (i in 1:10) {
  print(means[i])
}


#par(mfrow=c(2,2))
#plot(lm.fit)
#par(mfrow=c(1,1))


lm.fit = lm(Karn ~ treat, data = VA)
lm.fit2 = lm(Karn ~ treat, data = VA)
summary(lm.fit)
str(VA)








########################################
##########  DECISION TREE  #############
########################################

mean_test_prediction_error = 0
VA_data = subset(VA, select = -c(treat))
library(tree)
set.seed(1)

#Fit the decision tree on the training set and calculates the test prediction error 10 times
for(i in 1:10)
{
  #Splits 80% of data for training
  train = sample(1:nrow(VA_data),nrow(VA_data)*.8)
  
  #Construct tree
  tree.VA = tree(stime~., data=VA_data, subset=train)
  summary(tree.VA)
  tree.VA
  plot(tree.VA)
  text(tree.VA, pretty=0)
  
  #Pruning does not produce an improvement to the tree
  cv.VA = cv.tree(tree.VA)
  plot(cv.VA$size, cv.VA$dev, type='b')
  #prune.VA = prune.tree(tree.VA, best=1)
  #plot(prune.VA)
  #text(prune.VA, pretty=0)
  
  #Calculate mean test prediction error (remaining 20% of data for testing)
  yhat = predict(tree.VA, newdata=VA_data[-train,])
  test = VA_data[-train, "stime"]
  test
  plot(yhat, test)
  abline(0,1)
  test_prediction_error = mean((yhat-test)^2)
  mean_test_prediction_error = mean_test_prediction_error + test_prediction_error
  mean_test_prediction_error
}

#Mean test prediction error
mean_test_prediction_error = mean_test_prediction_error/10
mean_test_prediction_error








########################################
##########  BAGGING  ###################
########################################
# Conduct bagging for regression tree to
# predict survival time and compare test error.

mean_test_prediction_error = 0

# if we remove column of row numbers in excel
VA_data = subset(VA, select = -c(treat))

library(randomForest)
set.seed(1)


for (i in 1:10) 
{
  train = sample(1:nrow(VA_data),nrow(VA_data)*.8)
  
  bag.VA = randomForest(stime~., data=VA_data,
                        subset=train, mtry=6,
                        importance=TRUE)
  summary(bag.VA)
  bag.VA
  
  yhat.bag = predict(bag.VA, newdata=VA_data[-train,])
  test = VA_data[-train, "stime"]
  
  test_prediction_error = mean((yhat.bag-test)^2)
  mean_test_prediction_error = mean_test_prediction_error + test_prediction_error
  
}

importance(bag.VA)
varImpPlot(bag.VA, main = "importance")

mean_test_prediction_error = mean_test_prediction_error/10
mean_test_prediction_error

