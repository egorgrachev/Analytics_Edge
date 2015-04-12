setwd("AnalyticsEdge/Assigment3/")
data = read.csv("AnalyticsEdge/Assigment3/gerber.csv")
summary(data)
str(data)
tapply(data$voting, data$voting == 1, sum)
108696 / 344084
table(data$voting)
table(data$hawthorne, data$voting)[2,2] / table(data$hawthorne)[2]
table(data$civicduty, data$voting)[2,2] / table(data$civicduty)[2]
table(data$neighbors, data$voting)[2,2] / table(data$neighbors)[2]
table(data$self, data$voting)[2,2] / table(data$self)[2]

tapply(data$voting, data$hawthorne, mean)

model = glm(voting ~ hawthorne + civicduty + self + neighbors, data = data, family = 'binomial')
summary(model)
predicted = predict(model, type = "response") 
table(data$voting, predicted > 0.5)
234388 / (235388 + 108696)
(51966+134513) / (134513 + 100875 + 56730 + 51966)

library(ROCR)
ROCRpred = prediction(predicted, data$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ hawthorne + civicduty + self + neighbors, data = data)
prp(CARTmodel)
summary(CARTmodel)

CARTmodel2 = rpart(voting ~ hawthorne + civicduty + self + neighbors, data = data, cp = 0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ hawthorne + civicduty + self + neighbors + sex, data = data, cp = 0.0)
prp(CARTmodel3)

CARTcontrol = rpart(voting ~ control, data = data, cp = 0.0)
CARTcontrolsex = rpart(voting ~ control + sex, data = data, cp = 0.0)
prp(CARTcontrol)
summary(CARTcontrol)
abs(0.3400004 - 0.2966383)
prp(CARTcontrolsex, digits = 6)

LRcontrolsex = glm(voting ~ control + sex, data = data, family = 'binomial')
summary(LRcontrolsex)
Posibilities = data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
predict(LRcontrolsex, newdata = Posibilities, type = "response" )

LRmodel2 = glm(voting ~ control + sex + sex:control, data = data, family='binomial')
summary(LRmodel2)
predict(LRmodel2, newdata=Posibilities, type="response")
