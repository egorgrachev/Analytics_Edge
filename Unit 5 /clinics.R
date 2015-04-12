trials = read.csv("AnalyticsEdge/Unit 5 /clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)
nrow(subset(trials, nchar(trials$abstract) == 0))
a = subset(trials, nchar(trials$title) == min(nchar(trials$title)))
a$title

library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

dtmTitle
dtmAbstract

sort(colSums(dtmAbstract))
summary(dtmAbstract)
which.max(colSums(dtmAbstract))


colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)

table(train$trial)
730/(730+572)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)
pred = predict(trialCART, newdata=train)
pred
pred = pred[,2]
table(train$trial, pred>0.5)
(631+441) / (631+99+131+441)

predTest = predict(trialCART, newdata=test)
predTest = predTest[,2]
table(test$trial, predTest>0.5)

library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
# Compute AUC
performance(predROCR, "auc")@y.values
