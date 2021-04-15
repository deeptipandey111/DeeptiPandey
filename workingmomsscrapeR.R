# for data wrangling
library(tidyr)
library(stringr)
library(magrittr)
library(dplyr)
library(lubridate)
library(tm)
# for sentiment analysis
library(tidytext)
require(quanteda)
require(readtext)

# for visualization
library(ggplot2)
library(ggridges)
library(wordcloud)

setwd("~/Desktop/waapas/da")
df <- read.csv("workingmomfirst1000.csv", 
                         header=TRUE, stringsAsFactors=FALSE) 
toks <- tokens_remove(tokens(df$body, remove_punct = TRUE), stopwords("english"))

ngram_toks<- tokens_ngrams(toks, n = 2:6)

doc_term_matrix <- quanteda::dfm(ngram_toks,
                                 tolower = TRUE,
                                 stem = FALSE,
                                 remove = stopwords("english"))
topFeatures <- topfeatures(doc_term_matrix, 100)
topFeatures
#bigrams <- dfm(df$body, ngrams=2, ignoredFeatures=c(stopwords("english")))



df %>% glimpse()
titles <- df[["title"]]
titlecorpus <- Corpus(VectorSource(titles))

titlecorpus <- titlecorpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
titlecorpus <- tm_map(titlecorpus, content_transformer(tolower))
titlecorpus <- tm_map(titlecorpus, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(titlecorpus) 

matrix <- as.matrix(dtm) 

words <- sort(rowSums(matrix),decreasing=TRUE) 
dataframe <- data.frame(word = names(words),freq=words)
topfeatures(dfm(titlecorpus, ngrams = 2, verbose = FALSE))

set.seed(1234)
wordcloud(words = dataframe$word, freq = dataframe$freq, min.freq = 1,           max.words=200, random.order=FALSE, rot.per=0.35,            colors=brewer.pal(8, "Dark2"))
