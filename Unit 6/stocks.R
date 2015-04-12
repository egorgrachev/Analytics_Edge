stocks = read.csv("StocksCluster.csv")
table(stocks$PositiveDec)
summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
prediction = predict(StocksModel, newdata=stocksTrain, type="response")
table(stocksTrain$PositiveDec, prediction>0.5)
((1427+312) / (1427+150+1585+312)
(990+3640) / (990+2689+787+3640)

table(stocksTest$PositiveDec)
1897/(1577+1897)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

set.seed(144)
KMC = kmeans(normTrain, centers = 3)
str(KMC)

library(flexclust)
KMC.kcca = as.kcca(KMC, normTrain)
clusterTrain = predict(KMC.kcca)
clusterTest = predict(KMC.kcca, newdata=normTest)

stockTrain1 = subset(stocksTrain, clusterTrain == 1)
stockTrain2 = subset(stocksTrain, clusterTrain == 2)
stockTrain3 = subset(stocksTrain, clusterTrain == 3)

stockTest1 = subset(stocksTest, clusterTest == 1)
stockTest2 = subset(stocksTest, clusterTest == 2)
stockTest3 = subset(stocksTest, clusterTest == 3)

StocksModel1 = glm(PositiveDec ~ ., data=stockTrain1, family="binomial")
prediction1 = predict(StocksModel1, newdata=stockTest1, type="response")
StocksModel2 = glm(PositiveDec ~ ., data=stockTrain2, family="binomial")
prediction2 = predict(StocksModel2, newdata=stockTest2, type="response")
StocksModel3 = glm(PositiveDec ~ ., data=stockTrain3, family="binomial")
prediction3 = predict(StocksModel3, newdata=stockTest3, type="response")
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

table(stockTest1$PositiveDec, prediction1>0.5)
(30+774) / (30+471+23+774)
table(stockTest2$PositiveDec, prediction2>0.5)
table(stockTest3$PositiveDec, prediction3>0.5)

AllPredictions = c(prediction1, prediction2, prediction3)
AllTest = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)

table(AllTest, AllPredictions>0.5)
