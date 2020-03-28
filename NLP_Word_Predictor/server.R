#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(NLP)
library(tm)

library(dict)

library(plyr)
library(dplyr)

library(ggplot2)
library(plotly)


library(shiny)

serialize <- function(pList){
    newDict <- dict();
    for (i in 1:length(pList)) {
        if(!(i %% 2 == 0)){
            newDict[[ pList[[i]] ]] <- pList[[i + 1]]
        }
    }
    
    return(newDict)
}


gram2List <- readRDS("2gram_Dictionary.rds");
gram3List <- readRDS("3gram_Dictionary.rds");
gram4List <- readRDS("4gram_Dictionary.rds");

gram2Dict <- serialize(gram2List);
gram3Dict <- serialize(gram3List);
gram4Dict <- serialize(gram4List);

rm(gram2List);
rm(gram3List);
rm(gram4List);


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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    observeEvent( input$predict,{
        predction <- predict(input$caption);
        
        if(!is.null(predction)){
            output$no_match = renderText("");
            
            top3 <- getTop3(predction); 
            input_text <- input$caption;
            
            output$user_input = renderText(input_text);
            
            output$prediction1 = renderText({as.character(top3[1])});
            output$prediction2 = renderText({as.character(top3[2])});
            output$prediction3 = renderText({as.character(top3[3])});
            
            output$plot <- renderPlotly({
                createBarPlot(predction);
            });
        }else{
            output$no_match = renderText("No Match");
        }
        
    });
    

})
