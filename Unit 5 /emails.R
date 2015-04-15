emails = read.csv("Analytics_Edge/Unit 5 /emails.csv",stringsAsFactors=FALSE)

install.packages("SnowballC")
library(tm)

sw = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, sw)
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
emailSparse = make.names(emailsSparse)

which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam
table(emailsSparse$spam)

a=subset(emailsSparse, emailsSparse$spam == 1)
sort(colSums(a))

emailsSparse$spam = as.factor(emailsSparse$spam)


library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl==TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data=train, family = "binomial")

library(rpart)
spamCART = rpart(spam ~ ., data=train, method="class")

library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., as.factor(train))

predLog = predict(spamLog, test, type="response")
table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)
table(test$spam, predLog > 0.5)
summary(spamLog)

library(rpart.plot)
prp(spamCART)
table(train$spam, predLog > 0.5)

library(ROCR)
predROCR = prediction(predLog, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values

predCART = predict(spamCART, test)[,2]
table(test$spam, predCART>0.5)

library(ROCR)
predROCR = prediction(predCART, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
performance(predROCR, "auc")@y.values
