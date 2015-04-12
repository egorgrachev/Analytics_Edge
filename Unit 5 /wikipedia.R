wiki = read.csv("AnalyticsEdge/Unit 5 /wiki.csv", stringsAsFactors=FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
sw = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")


library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added),readerControl = list(language="english"))
#corpusAdded = tm_map(corpusAdded, content_transformer(tolower))
corpusAdded = tm_map(corpusAdded, PlainTextDocument, lazy=TRUE)
corpusAdded = tm_map(corpusAdded, removeWords, sw)
corpusAdded = tm_map(corpusAdded, stemDocument, lazy = TRUE)

dtmAdded = DocumentTermMatrix(corpusAdded)
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)

corpusRemoved = Corpus(VectorSource(wiki$Removed))
#corpusAdded = tm_map(corpusAdded, content_transformer(tolower))
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument, lazy=TRUE)
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument, lazy = TRUE)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)
summary(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal


library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

library(rpart)
library(rpart.plot)
VandalCART = rpart(Vandal ~ ., data=train, method="class")
prp(VandalCART)
pred = predict(VandalCART, newdata=test)
pred.prob = pred[,2]
table(test$Vandal, pred.prob > 0.5)


wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
VandalCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
pred = predict(VandalCART2, newdata=wikiTest2)
pred.prob = pred[,2]
table(test$Vandal, pred.prob > 0.5)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
VandalCART3 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
pred = predict(VandalCART3, newdata=wikiTest2)
pred.prob = pred[,2]
table(test$Vandal, pred.prob > 0.5)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain2 = subset(wikiWords3, spl==TRUE)
wikiTest2 = subset(wikiWords3, spl==FALSE)
VandalCART4 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
pred = predict(VandalCART4, newdata=wikiTest2)
pred.prob = pred[,2]
table(test$Vandal, pred.prob > 0.5)
