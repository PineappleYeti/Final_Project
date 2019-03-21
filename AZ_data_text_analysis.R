library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(tidytext)
library(wordcloud)

##READ IN THE DATA
arizona = read.csv("/Volumes/Not Your Hard Drive/Springboard/Yelp Data/yelp_dataset/random300.csv", stringsAsFactors = FALSE)
starReviews = arizona %>% select(text, stars.y)
starReviews$Negative <- as.factor(starReviews$stars.y < 3)

##WORD CLOUD
unnestedTokens = starReviews %>% unnest_tokens(word, text) %>% anti_join(stop_words)
counted = unnestedTokens %>% count(word, sort = TRUE)
wordcloud(counted$word, counted$n, max.words = 75, colors = brewer.pal(8, "Accent"))
#NEGATIVE
negUnnestedTokens = starReviews %>% filter(Negative == TRUE) %>% unnest_tokens(word, text) %>% anti_join(stop_words)
negCounted = negUnnestedTokens %>% count(word, sort = TRUE)
wordcloud(negCounted$word, negCounted$n, max.words = 75, colors = brewer.pal(8, "Accent"))
#POSITIVE
posUnnestedTokens = starReviews %>% filter(Negative == FALSE) %>% unnest_tokens(word, text) %>% anti_join(stop_words)
posCounted = posUnnestedTokens %>% count(word, sort = TRUE)
wordcloud(posCounted$word, posCounted$n, max.words = 75, colors = brewer.pal(8, "Accent"))

##VISUALIZATION
posNeg = counted %>% inner_join(get_sentiments("bing"))
posNeg = select(posNeg, word, sentiment, n)
posNeg %>% 
  group_by(sentiment) %>% 
  top_n(20) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip()

##TEXT ANALYSIS
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

##SPLITTING THE DATA
split = sample.split(reviewsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(reviewsSparse, split == TRUE)
testSparse = subset(reviewsSparse, split == FALSE)

##CREATING THE MODEL
reviewCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(reviewCART)
predictCart = predict(reviewCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCart)
##(66862+7720)/(66862+2203+13215+7720) = 0.8286889
table(testSparse$Negative)
##230218/(230218+69782) = 0.7673933
