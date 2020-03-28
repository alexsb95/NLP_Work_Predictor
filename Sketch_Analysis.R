library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(plotly)

# Task 0
#   Obtaining the data
setwd("~/Coursera/Data Science/Data Science Capstone")
con <- file("en_US.twitter.txt", "r") 
twitterText <- readLines(con,5000) 
close(con)

set.seed(1345)
index <- rbinom(n = length(twitterText), size = 1, prob = 0.5)
twitterText <- twitterText[as.logical(index)]
# Task 1
#   Profanity filtering
# Credit to https://www.cs.cmu.edu/~biglou/resources/
profanity <- readLines("bad-words.txt")

twitText_clean <- unlist(lapply(twitterText, function(i) removeWords(i, profanity))) # Remove profanity
# library(cld3)
# translation <- detect_language(twitText_clean)

twitText_clean <- iconv(twitText_clean, "latin1", "ASCII//TRANSLIT") # Remove foreing characters
#ddd <- list(wordFreq[,1])

#   Tokenization
twitCorpus <- VCorpus(VectorSource(twitText_clean))

twitCorpus_clean <- tm_map(twitCorpus,removeNumbers) #removing numbers
twitCorpus_clean <- tm_map(twitCorpus_clean,content_transformer(tolower)) #converting to lower case letters
twitCorpus_clean <- tm_map(twitCorpus_clean,removePunctuation) #remving punctuation

twitCorpus_clean <- tm_map(twitCorpus_clean,stripWhitespace)

twit_tdm <- TermDocumentMatrix(twitCorpus_clean) #Tokenization

#findFreqTerms(docs_dtm, 10)

# Task 2
#   Exploratory analysis
 
word_matrix <- sort(rowSums(as.matrix(twit_tdm)),decreasing=TRUE) #Order the the words
wordFreq <- data.frame(word = names(word_matrix),freq=word_matrix)
rownames(wordFreq) <- NULL
#wordFreq = data.frame(freq = sort(colSums(as.matrix(twit_tdm)), decreasing=TRUE))

head(wordFreq,10) #Top 10 of frecuent word

freqHist <- plot_ly(x = ~wordFreq$freq, type = "histogram") # Distribution of the word freq

wordcloud(wordFreq$word, wordFreq$freq, max.words=500, min.freq = 2, colors=brewer.pal(5, "Dark2")) #Wordcloud

twitAsso <- unlist(lapply(wordFreq[1:10,1], function(i) findAssocs(twit_tdm, terms = as.character(i), corlimit = 0.5))) 

#   Understand frequencies of words and word pairs

#BI-GRAM
BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

twit_tdm2 <- TermDocumentMatrix(twitCorpus_clean, control = list(tokenize = BigramTokenizer)) # Get the bi-gram

word_matrix2 <- sort(rowSums(as.matrix(twit_tdm2)),decreasing=TRUE)
wordFreq2 <- data.frame(word = names(word_matrix2),freq=word_matrix2)
rownames(wordFreq2) <- NULL
head(wordFreq2,10)

wordcloud(wordFreq2$word, wordFreq2$freq, max.words=500, min.freq = 2, colors=brewer.pal(8, "Dark2")) #Wordcloud

#TRI-GRAM
TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

twit_tdm3 <- TermDocumentMatrix(twitCorpus_clean, control = list(tokenize = TrigramTokenizer)) # Get the bi-gram

word_matrix3 <- sort(rowSums(as.matrix(twit_tdm3)),decreasing=TRUE)
wordFreq3 <- data.frame(word = names(word_matrix3),freq=word_matrix3)
rownames(wordFreq3) <- NULL
head(wordFreq3,10)

wordcloud(wordFreq3$word, wordFreq3$freq, max.words=500, min.freq = 2, colors=brewer.pal(5, "Dark2")) #Wordcloud


floor(nrow(wordFreq)*0.5) # numbers of words to have 50% of the dictonary

floor(nrow(wordFreq)*0.9) # numbers of words to have 90% of the dictonary



