# 
# filenames <- "../quiz.txt"
# print(filenames)
# LinesToRead <- 100
# mytext <- NULL
# cleaned_text <- NULL
# 
# for (filename in filenames){
#         mytext <- c(mytext, list(paste(readLines(file(filename), LinesToRead, encoding = "UTF-8", warn = FALSE), collapse="\n")))
#         close(file(filename))
# }

AposConvert <- function(rawText){
        for (i in 1:nrow(convAposWord)){
                searchPhrase <- convAposWord$apostropheWords[i]
                rawText <- gsub(pattern = searchPhrase, x=rawText, replacement = convAposWord$longForm[i])
        }
        return(rawText)
}

removesingleChars <- function(rawText){
        for (i in 1:length(singlechars)){
                searchPhrase <- paste0('\\<', singlechars[i], '\\>')
                rawText <- gsub(pattern = searchPhrase, x=rawText, replacement = " ")
        }
        return(rawText)
}

TextCleaner <- function(text_data){
        #Remove ... with just a single .
        text_data <- gsub(pattern="\\.+", x=text_data, replacement = '.')
        #Swap all sentences ends with split code 'ootoo'
        text_data <- gsub(pattern=';|\\.|!|\\?', x=text_data, replacement='ootoo')
        text_data <- gsub(pattern="[^A-Za-z']+", x=text_data, replacement = ' ')
        text_data <- tolower(text_data)
        #Convert Apostrophe words to its long form
        text_data <- AposConvert(text_data)
        #remove single chars that may have been created by removing punctuation
        text_data <- gsub(pattern="[[:punct:]]", x=text_data, replacement = ' ')
        text_data <- removesingleChars(text_data)
        #remove leading whitespace
        text_data <- gsub(pattern="^\\s+", x=text_data, replacement = ' ')
        #remove trailing space
        text_data <- gsub(pattern="\\s+", x=text_data, replacement = ' ')
        #split sentences by split code
        text_data <- unlist(strsplit(x=text_data, split='ootoo', fixed = TRUE))
        
        return(text_data)
}

# cleanedText <- TextCleaner(mytext)
# print(cleanedText)