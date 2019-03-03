library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)

arizona = read.csv("/Volumes/Not Your Hard Drive/Springboard/Yelp Data/yelp_dataset/AZ300.csv", stringsAsFactors = FALSE)
starReviews = arizona %>% select(text, stars.y)
starReviews$Negative <- as.factor(starReviews$stars.y < 3)

corpus = Corpus(VectorSource(starReviews$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("yelp", stopwords("english")))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.98)
reviewsSparse = as_tibble(as.matrix(sparse))
colnames(reviewsSparse) = make.names(colnames(reviewsSparse))
reviewsSparse$Negative = starReviews$Negative

split = sample.split(reviewsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(reviewsSparse, split == TRUE)
testSparse = subset(reviewsSparse, split == FALSE)

reviewCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(reviewCART)
predictCart = predict(reviewCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCart)
##Add the TT and FF (1&4) and divide by the total number
##(66862+7720)/(66862+2203+13215+7720) = 0.8286889

table(testSparse$Negative)
##Divide the false by the true + false
##230218/(230218+69782) = 0.7673933

