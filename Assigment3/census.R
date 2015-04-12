data = read.csv("AnalyticsEdge/Assigment3/census.csv")
str(data)

set.seed(2000)
spl = sample.split(data$over50k, SplitRatio = 0.6)
Train = subset(data, spl==TRUE)
Test = subset(data, spl==FALSE)

GLM = glm(over50k ~ ., data=Train, family="binomial")
summary(GLM)
PredictionGLM = predict(GLM, newdata=Test)
table(Test$over50k, PredictionGLM > 0.5)
(9351+1515) / (9351+362+1563+1515)
table(Test$over50k)

library(ROCR)
ROCRprediction = prediction(PredictionGLM, Test$over50k)
as.numeric(performance(ROCRprediction, "auc")@y.values)

CART = rpart(over50k ~ ., data=Train, method="class")
PredCART = predict(CART, newdata=Test)
prp(CART)
table(Test$over50k, PredCART)
(9243+1596) / (9243+470+1482+1596)

ROCRprediction = prediction(PredCART, Test$over50k)
as.numeric(performance(ROCRprediction, "auc")@y.values)


set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]
set.seed(1)
RF = randomForest(over50k ~ ., data=trainSmall)
PredRF = predict(RF, newdata=Test)
table(Test$over50k, PredRF)
(9586+1093) / (9586+127+1985+1093)

vu = varUsed(RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))
varImpPlot(RF)


library(caret)
library(e1071)
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data=Train, method="rpart", tuneGrid=cartGrid)

superCART = rpart(over50k ~ ., data=Train, method='class', cp=0.002)
predsuperCart = predict(superCART, Test, type='class')
table(Test$over50k, predsuperCart)
(9178+1838) / (9178+535+1240+1838)

prp(superCART)
