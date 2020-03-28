library(NLP)
library(tm)
library(SnowballC)

library(ggplot2)
library(plotly)

library(dict)
library(plyr)
library(dplyr)



rmWords <- function(pStr, pStopwords) {
    x <- unlist(strsplit(tolower(pStr), " "))
    return (paste(x[!x %in% pStopwords], collapse = " "))
}

#   Read the files
setwd("~/Coursera/Data Science/Data Science Capstone")

twitterCon <- file("en_US.twitter.txt", "r") 
newsCon <- file("en_US.news.txt", "r")
blogsCon <- file("en_US.blogs.txt", "r") 

twitterText <- readLines(twitterCon, skipNul=T) 
newsText <- readLines(newsCon, skipNul=T) 
blogsText <- readLines(blogsCon, skipNul=T) 

close(twitterCon)
close(newsCon)
close(blogsCon)

textList <- c(twitterText,newsText,blogsText)

#   Extract 10% for the sample
set.seed(1345)
index <- rbinom(n = length(textList), size = 1, prob = 0.01)
textList <- textList[as.logical(index)]

#   Extract 60% for trainig 
set.seed(1345)
index <- rbinom(n = length(textList), size = 1, prob = 0.6)
trainingList <- textList[as.logical(index)]


#   Profanity filtering
# Credit to https://www.cs.cmu.edu/~biglou/resources/
profanity <- readLines("bad-words.txt")

trainingList <- unlist(lapply(trainingList, function(x) rmWords(x, profanity)))  # Remove profanity
trainingList <- iconv(trainingList, "latin1", "ASCII//TRANSLIT") # Remove foreing characters

#   Tokenization
textCorpus <- VCorpus(VectorSource(trainingList))

textCorpus <- tm_map(textCorpus,removeNumbers) #removing numbers
textCorpus <- tm_map(textCorpus,content_transformer(tolower)) #converting to lower case letters
textCorpus <- tm_map(textCorpus,removePunctuation) #remving punctuation

textCorpus <- tm_map(textCorpus,stripWhitespace)

#rm(textList)
#tic=proc.time()[3]
#toc=proc.time()[3]

#   Generic functions

## Frequency table
createFreqTable <- function(pNGram)
{
    NGramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), pNGram), paste, collapse = " "), use.names = FALSE)
    
    ngramTDM <- TermDocumentMatrix(textCorpus, control = list(tokenize = NGramTokenizer)) # Get the bi-gram
    
    ngramMatrix <- slam::row_sums(ngramTDM)
    ngramMatrix <- ngramMatrix[order(-ngramMatrix)]
    
    ngramFreq <- data.frame(word = names(ngramMatrix),freq=ngramMatrix)
    rownames(ngramFreq) <- NULL
    
    return(ngramFreq)
}

## Probability Dictionary
createProbDict <- function(pTable){
    #The table of frecuency distribution
    freqDist <- plyr::count(pTable$freq)
    freqDist <- rbind(c(0,freqDist[1,2]),freqDist) ## Add the posible amount of unseen observation
    names(freqDist) <- c("r","N_r")
    freqDist$prob = 0
    renorm = 0
    
    ## Calculate the prob by using Good Turning
    ## (r+1)*(N_r+1/N*N_r)
    N <- sum(pTable$freq)
    
    for (i in 1:nrow(freqDist)){
        if(i == nrow(freqDist)){
            freqDist[i,3] = freqDist[i,1] / N; ## We use MLE
        }else{
            freqDist[i,3] = (freqDist[i,1] + 1) * (freqDist[i+1,2]/(N*freqDist[i,2]));
        }
        renorm = renorm + (freqDist[i,3]*freqDist[i,2]);
    }
    
    ## We renormalize weights
    ## sum(P_n*N_n)
    freqDist$prob = freqDist$prob * renorm;
    
    ## Create the dictionary
    probDict <- dict();
    for (i in 1:nrow(freqDist)) {
        probDict[[freqDist[i,1]]] <- freqDist[i, 3]
    }
    
    return(probDict)
}

#   One Gram

gram1TDM <- TermDocumentMatrix(textCorpus) #Tokenization

gram1Matrix <- slam::row_sums(gram1TDM)
gram1Matrix <- gram1Matrix[order(-gram1Matrix)]

gram1Freq <- data.frame(word = names(gram1Matrix),freq=gram1Matrix)
rownames(gram1Freq) <- NULL

rm(gram1TDM)
rm(gram1Matrix)

gram1ProbDict <- createProbDict(gram1Freq);



#   Bi Gram

gram2Freq <- createFreqTable(2);

gram2ProbDict <- createProbDict(gram2Freq);

## 2-Gram Dictionary

gram2Dict <- dict()
create2GramDict <- function(pRow, pSplitIndex){
    wordList = unlist(strsplit(pRow[1]," "))
    key = paste(wordList[1:(pSplitIndex-1)], collapse=" ")
    value = gram2Dict$get(key, data.frame(word = "N/A", prob = -1))
    
    if(value[1,2] == -1){
        gram2Dict[[key]] <- data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram2ProbDict$get(as.numeric(pRow[2]),-1)) )
    }else{
        gram2Dict[[key]] <- rbind(value, data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram2ProbDict$get(as.numeric(pRow[2]),-1)) ) )
    }
} 

tic=proc.time()[3];
apply(gram2Freq, 1, FUN = function(x) create2GramDict(x,2) );
toc=proc.time()[3];

gram2List <- deserialize(gram2Dict)
saveRDS(gram2List, "2gram_Dictionary.rds")
rm(gram2List)

#   Tri Gram

gram3Freq <- createFreqTable(3);

gram3ProbDict <- createProbDict(gram3Freq);

## 3-Gram Dictionary

gram3Dict <- dict()
create3GramDict <- function(pRow, pSplitIndex){
    wordList = unlist(strsplit(pRow[1]," "))
    key = paste(wordList[1:(pSplitIndex-1)], collapse=" ")
    value = gram3Dict$get(key, data.frame(word = "N/A", prob = -1))

    if(value[1,2] == -1){
        gram3Dict[[key]] <- data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram3ProbDict$get(as.numeric(pRow[2]),-1)) )
    }else{
        gram3Dict[[key]] <- rbind(value, data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram3ProbDict$get(as.numeric(pRow[2]),-1)) ) )
    }
} 
tic=proc.time()[3]
apply(gram3Freq, 1, FUN = function(x) create3GramDict(x,3) );
toc=proc.time()[3]

#   Qua Gram

gram4Freq <- createFreqTable(4);

gram4ProbDict <- createProbDict(gram4Freq);

## 4-Gram Dictionary

gram4Dict <- dict()
create4GramDict <- function(pRow, pSplitIndex){
    wordList = unlist(strsplit(pRow[1]," "))
    key = paste(wordList[1:(pSplitIndex-1)], collapse=" ")
    value = gram4Dict$get(key, data.frame(word = "N/A", prob = -1))
    
    if(value[1,2] == -1){
        gram4Dict[[key]] <- data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram4ProbDict$get(as.numeric(pRow[2]),-1)) )
    }else{
        gram4Dict[[key]] <- rbind(value, data.frame(word = wordList[pSplitIndex], prob = as.numeric(gram4ProbDict$get(as.numeric(pRow[2]),-1)) ) )
    }
} 


apply(gram4Freq, 1, FUN = function(x) invisible(create4GramDict(x,4)) );


##  Saving and loading

#saveRDS(gram2Dict, "2gram_Dictionary.rds")
#saveRDS(gram3Dict, "3gram_Dictionary.rds")
#saveRDS(gram4Dict, "4gram_Dictionary.rds")

#gram2Dict <- readRDS("2gram_Dictionary.rds")
#gram3Dict <- readRDS("3gram_Dictionary.rds")
#gram4Dict <- readRDS("4gram_Dictionary.rds")


deserialize <- function(pDict){
    nGramList <- list();
    index <- 1
    for (keys in pDict$keys()) {
        nGramList[[index]] <- keys;
        index = index + 1;
        nGramList[[index]] <- pDict[[keys]];
        index = index + 1;
    }
    
    return(nGramList)
}


serialize <- function(pList){
    newDict <- dict();
    for (i in 1:length(pList)) {
        if(!(i %% 2 == 0)){
            newDict[[ pList[[i]] ]] <- pList[[i + 1]]
        }
    }
    
    return(newDict)
}


gram3List <- deserialize(gram2Dict)
saveRDS(gram3List, "3gram_Dictionary.rds")
rm(gram3List)

gram4List <- deserialize(gram2Dict)
saveRDS(gram4List, "4gram_Dictionary.rds")
rm(gram4List)


##      Pedict      ##

getTop3 <- function(pPred){
    # Get top 3 with hightest probability
    if(nrow(pPred) > 3){
        pPred <- dplyr::sample_n(pPred, size = 3, weight = pPred$prob)
        pPred <- pPred[order(-pPred$prob),]
        #pPred <- pPred[1:5,]
    }
    
    return(pPred[,1])
}

preProccessLine <- function(pLine){
    pLine <- removeNumbers(pLine);
    pLine <- removePunctuation(pLine);
    pLine <- tolower(pLine);
    pLine <- stripWhitespace(pLine);
    
    return(unlist(strsplit(pLine," ")));
}

predict <- function(pLine){
    
    wordlist <- preProccessLine(pLine);
    wordLength = length(wordlist);
    
    if(wordLength >= 3){
        # Try the 4-gram
        key <- paste(wordlist[(wordLength-2):wordLength], collapse=" ");  #print(key); print("1");
        pred <- gram4Dict$get(key,data.frame(word = "N/A", prob = -1));
        
        if(pred[1,2] != -1){
            
            return(pred)
            
        }else{
            
            # Try the 3-gram
            key <- paste(wordlist[(wordLength-1):wordLength], collapse=" ");  #print(key); print("2");
            pred <- gram3Dict$get(key,data.frame(word = "N/A", prob = -1));
            
            if(pred[1,2] != -1){
                
                return(pred)
                
            }else{
                # Try the 2-gram
                key <- wordlist[wordLength];  #print(key);  print("3");
                pred <- gram2Dict$get(key,data.frame(word = "N/A", prob = -1));
                
                if(pred[1,2] != -1){
                    
                    return(pred)
                    
                }
                
            }
        }
        
        
    }else if (wordLength == 2){
        # Try the 3-gram
        key <- paste(wordlist[(wordLength-1):wordLength], collapse=" ");  #print(key); print("5");
        pred <- gram3Dict$get(key,data.frame(word = "N/A", prob = -1));
        
        if(pred[1,2] != -1){
            
            return(pred)
            
        }else{
            # Try the 2-gram
            key <- wordlist[wordLength];  #print(key);  print("6");
            pred <- gram2Dict$get(key,data.frame(word = "N/A", prob = -1));
            
            if(pred[1,2] != -1){
                
                return(pred)
                
            }
            
        }
        
    }else if (wordLength == 1){
        # Try the 2-gram
        key <- wordlist[wordLength];  #print(key);  print("7");
        pred <- gram2Dict$get(key,data.frame(word = "N/A", prob = -1));
        
        if(pred[1,2] != -1){
            
            return(pred);
            
        }
        
    }

        
    return(NULL);
    
}

createBarPlot <- function(pDataFrame){
    if( nrow(pDataFrame) > 10){
        pDataFrame <- pDataFrame[1:10,]
    }
    
    pDataFrame$word <- factor(pDataFrame$word, levels = unique(pDataFrame$word)[order(pDataFrame$prob, decreasing = TRUE)])
    fig <- plot_ly(
        data = pDataFrame,
        x = ~word,
        y = ~prob,
        type = "bar"
    ) %>%
        layout(title = "Top 10 words with higher probability")
}

resultPred <- predict("The guy in front of me just bought a pound of bacon, a bouquet, and a case of");
top3Pred <- getTop3(resultPred); 
barChart <- createBarPlot(resultPred);



##      Test set        ##

testList <- textList[as.logical(1-index)]


#   Profanity filtering
# Credit to https://www.cs.cmu.edu/~biglou/resources/
profanity <- readLines("bad-words.txt")

testList <- unlist(lapply(testList, function(x) rmWords(x, profanity)))  # Remove profanity
testList <- iconv(testList, "latin1", "ASCII//TRANSLIT") # Remove foreing characters

#   Tokenization
testCorpus <- VCorpus(VectorSource(testList))

testCorpus <- tm_map(testCorpus,removeNumbers) #removing numbers
testCorpus <- tm_map(testCorpus,content_transformer(tolower)) #converting to lower case letters
testCorpus <- tm_map(testCorpus,removePunctuation) #remving punctuation

testCorpus <- tm_map(testCorpus,stripWhitespace)


## Create the fequency table
testCreateFreqTable <- function(pNGram)
{
    NGramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), pNGram), paste, collapse = " "), use.names = FALSE)
    
    ngramTDM <- TermDocumentMatrix(testCorpus, control = list(tokenize = NGramTokenizer)) # Get the bi-gram
    
    ngramMatrix <- slam::row_sums(ngramTDM)
    ngramMatrix <- ngramMatrix[order(-ngramMatrix)]
    
    ngramFreq <- data.frame(word = names(ngramMatrix),freq=ngramMatrix)
    rownames(ngramFreq) <- NULL
    
    return(ngramFreq)
}


testGramFreq <- testCreateFreqTable(4);

## 4-Gram Dictionary


testCreateGramTable <- function(pTable, pSplitIndex){
    testTable <- NULL;
    
    
    
    for (i in 1:nrow(pTable)) {
        wordList <- unlist(strsplit(as.character(pTable[i,1])," "));
        key = paste(wordList[1:(pSplitIndex-1)], collapse=" ");
        
        if(is.null(testTable)){
            testTable <- data.frame(sentence = key, prediction = wordList[pSplitIndex] );
        }else{
            testTable <- rbind(testTable, data.frame(sentence = key, prediction = wordList[pSplitIndex] ) );
        }
    }
    
    return(testTable);

} 

testTable <- testCreateGramTable(testGramFreq,4);

print(points/nrow(testTable)*100);





