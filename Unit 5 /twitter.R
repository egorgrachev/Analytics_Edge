install.packages("tm")

tweets = read.csv("AnalyticsEdge/Unit 5 /tweets.csv", stringsAsFactors=FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
str(tweets)
table(tweets$Negative)
library(tm)
install.packages("SnowballC")
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]


frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq=100)
sparse = removeSparseTerms(frequencies, 0.995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
teatSparse = subset(trainSparse, split==FALSE)
