# Author: Tejash Panchal
# Word Prediction
# Coursera Data Science Capstone Project
# Licence: Please see LICENSE.md file for terms and condition
#
# This is the server side of a Shiny web application.
# First it cleans user input texts and converts to lower case.
# As user types, it examins each characters and provides hints for the word being typed.
# If it detects an empty space, the application sends last two typed words to
# appropriate function based on chosen algorithm to get next word prediction.
# Prediction function performs probability calculations and returns top 5 predictions.
# Server side passes those predictions to UI side and displays in table format.


library(shiny)

source("textCleaner.R")
source("prediction.R")

#Get hint to complete this word
hintThisWord <- function(searchString){
        hintWords <- NULL
        
        searchPhrase <- paste0('^\\<', searchString)
        hintWords_table <- subset(unigrams, grepl(searchPhrase, unigrams$word))
        hintWords <- as.character(head(hintWords_table$word, 5))

        return(hintWords)
}

shinyServer(function(input, output, session) {

        currentWord <- reactive({
                cleanedInputText <- NULL
                typedWords <- NULL
                
                userTyped <- input$inText
                Smoothing <- input$smoothing
                #print(Smoothing)
                #Inspect if the last char user typed is and empty space or not
                splittedChars <- unlist(strsplit(userTyped, ""))
                if (length(splittedChars) > 0) {
                        if (splittedChars[length(splittedChars)] == " "){
                                #cleanedInputText <- c(cleanedInputText, list(inputTextCleaner(paste(userTyped, collapse = " "))))
                                cleanedInputText <- c(cleanedInputText, list(TextCleaner(paste(userTyped, collapse = " "))))
                                typedWords <- unlist(strsplit(cleanedInputText[[1]], " "))
                                searchString <- paste(tail(typedWords, 2), collapse = " ")
                                if (length(unlist(strsplit(searchString, " "))) < 2){
                                        PredictWords <- getSingleWordPrediction(searchString)
                                } else {
                                        if (Smoothing == "Interpolation"){
                                                PredictWords <- getInterPollationPrediction(searchString)
                                        }
                                        if (Smoothing == "MLE"){
                                                PredictWords <- getMlePrediction(searchString)
                                        }
                                        if (Smoothing == "Laplace"){
                                                PredictWords <- getLaplacePrediction(searchString)
                                        }
                                        if (Smoothing == "GoodTuring"){
                                                PredictWords <- getGoodTuringPrediction(searchString)
                                        }
                                }
                                currentWord_table <- data.frame(
                                        "Suggestion 1" = PredictWords[1], 
                                        "Suggestion 2" = PredictWords[2], 
                                        "Suggestion 3" = PredictWords[3], 
                                        "Suggestion 4" = PredictWords[4], 
                                        "Suggestion 5" = PredictWords[5]
                                )
                        } else {
                                typedWords <- unlist(strsplit(userTyped, " "))
                                searchString <- paste(tail(typedWords, 1), collapse = " ")

                                hintWords <- hintThisWord(searchString)
                                currentWord_table <- data.frame(
                                        "Hint 1" = hintWords[1], 
                                        "Hint 2" = hintWords[2], 
                                        "Hint 3" = hintWords[3], 
                                        "Hint 4" = hintWords[4], 
                                        "Hint 5" = hintWords[5]
                                )
                        }
                        return(currentWord_table)
                }
        })
        
        output$wordHint <- renderDataTable({
                currentWord()
        })
        
        observeEvent(input$goButton, {
                updateTextInput(session, "inText", value = "")
        })        
})

