#Single word prediction
getSingleWordPrediction <- function(searchString){
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        singleWord_table <- subset(bigrams, grepl(searchPhrase, bigrams$word))
        
        #Now let's calculate probability of each bigrams
        probability <- NULL
        for (i in 1:nrow(singleWord_table)){
                thisWordFreq <- singleWord_table$frequency[i]
                probability <- c(probability, (thisWordFreq/sum(singleWord_table$frequency)))
        }
        #Now append probability column to words table
        singleWord_table$probability <- probability
        #print(head(singleWord_table))
        
        #Remove the searchString from the word list
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        singleWord_table$word <- gsub(searchPhrase, "", paste(singleWord_table$word))
        nextWords <- as.character(head(singleWord_table$word, 5))
        
        return(nextWords)
}

#MLE smoothing prediction
getMlePrediction <- function(searchString){
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        mleWord_table <- subset(trigrams, grepl(searchPhrase, trigrams$word))
        
        #Now let's calculate probability of each trigrams
        probability <- NULL
        for (i in 1:nrow(mleWord_table)){
                thisWordFreq <- mleWord_table$frequency[i]
                #print(mleWord_table$word[i])
                #Pml(Wi|Wi-2Wi-1) = Count(Wi-2Wi-1Wi)/Count(Wi-2Wi-1)
                #Probability of this word given previous two words = count of this word/count of previous two words
                probability <- c(probability, (thisWordFreq/sum(mleWord_table$frequency)))
        }
        #Now append probability column to trigramWords_table
        mleWord_table$probability <- probability
        #print(head(mleWord_table))
        
        #Remove the searchString from the word list
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        mleWord_table$word <- gsub(searchPhrase, "", paste(mleWord_table$word))
        nextWords <- as.character(head(mleWord_table$word, 5))
        
        return(nextWords)
}

#Laplace smoothing prediction
getLaplacePrediction <- function(searchString){
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        laplaceWord_table <- subset(trigrams, grepl(searchPhrase, trigrams$word))
        
        #Now let's calculate probability of each trigrams
        probability <- NULL
        for (i in 1:nrow(laplaceWord_table)){
                thisWordFreq <- laplaceWord_table$frequency[i]
                probability <- c(probability, ((thisWordFreq + 1)/(sum(laplaceWord_table$frequency) + nrow(trigrams))))
        }
        #Now append probability column to trigramWords_table
        laplaceWord_table$probability <- probability
        #print(head(laplaceWord_table))
        
        #Remove the searchString from the word list to provide next word only instead of trigram
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        laplaceWord_table$word <- gsub(searchPhrase, "", paste(laplaceWord_table$word))
        nextWords <- as.character(head(laplaceWord_table$word, 5))
        
        return(nextWords)
}

#Interpolation smoothing prediction
getInterPollationPrediction <- function(searchString){
        
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        trigramWords_table <- subset(trigrams, grepl(searchPhrase, trigrams$word))
        #print(searchString)
        #print(nrow(trigramWords_table))
        
        #Now let's calculate probability of each trigrams
        #myEM_trigram <- normalmixEM(trigrams$frequency, mu = c(0,1), sigma=c(1,1), sd.constr=c(1,1) )
        #myEM_trigram$lambda
        lambda1 <- 0.6
        probability <- NULL
        for (i in 1:nrow(trigramWords_table)){
                thisWordFreq <- trigramWords_table$frequency[i]
                probability <- c(probability, (lambda1 * (thisWordFreq/sum(trigramWords_table$frequency))))
        }
        #Now append probability column to trigramWords_table
        trigramWords_table$probability <- probability
        
        #Now let's find all possible bigrams based on the last word in searchString
        bigramWord <- NULL
        for (i in 1:nrow(trigramWords_table)){
                splittedWords <- unlist(strsplit(as.character(trigramWords_table$word[i]), " "))
                bigramWord <- c(bigramWord, paste(tail(splittedWords, 2), sep = " ", collapse = " "))
        }
        #Convert bigramWord into df so it will be easy to search the whole column
        search_table <- data.frame(
                "bigramWords" = bigramWord
        )
        selectedRows <- bigrams$word %in% search_table$bigramWords
        bigramWords_table <- bigrams[selectedRows,]
        #print(nrow(bigramWords_table))
        #print(bigramWords_table)
        
        #Now let's calculate probability of each bigrams
        lambda2 <- 0.3
        probability <- NULL
        for (i in 1:nrow(bigramWords_table)){
                thisWordFreq <- bigramWords_table$frequency[i]
                probability <- c(probability, (lambda2 * (thisWordFreq/sum(bigramWords_table$frequency))))
        }
        #Now append probability column to bigramWords_table
        bigramWords_table$probability <- probability
        
        #Now let's find all possible prediction words unigrams
        unigramWord <- NULL
        for (i in 1:nrow(bigramWords_table)){
                splittedWords <- unlist(strsplit(as.character(bigramWords_table$word[i]), " "))
                unigramWord <- c(unigramWord, paste(tail(splittedWords, 1), sep = " ", collapse = " "))
        }
        #Convert unigramWord into df so it will be easy to search the whole column
        search_table <- data.frame(
                "unigramWords" = unigramWord
        )
        selectedRows <- unigrams$word %in% search_table$unigramWords
        unigramWords_table <- unigrams[selectedRows,]
        #print(nrow(unigramWords_table))
        #print(unigramWords_table)                
        
        #Now let's calculate probability of each bigrams
        lambda1 <- 0.1
        probability <- NULL
        for (i in 1:nrow(unigramWords_table)){
                thisWordFreq <- unigramWords_table$frequency[i]
                probability <- c(probability, (lambda1 * (thisWordFreq/sum(unigramWords_table$frequency))))
        }
        #Now append probability column to unigramWords_table
        unigramWords_table$probability <- probability
        
        #Now let's find each unigram from bigram and trigram and extract it's probability
        cumulative_prob <- NULL
        for (i in 1:nrow(unigramWords_table)){
                searchPhrase <- paste0('\\<', as.character(unigramWords_table$word[i]), '$')
                tri_row <- subset(trigramWords_table, grepl(searchPhrase, trigramWords_table$word))
                bi_row <- subset(bigramWords_table, grepl(searchPhrase, bigramWords_table$word))
                uni_row <- subset(unigramWords_table, grepl(searchPhrase, unigramWords_table$word))
                cumulative_prob <- c(cumulative_prob, (tri_row$probability + bi_row$probability + uni_row$probability))
        }
        #Append cumulative_prob column to unigramWords_table
        unigramWords_table$cumulative_prob <- cumulative_prob
        nextWords <- as.character(head(unigramWords_table$word, 5))
        #print(nextWords)
        return(nextWords)
}

#Good Turing word prediction
getGoodTuringPrediction <- function(searchString){
        ##Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        trigramWords_table <- subset(trigrams, grepl(searchPhrase, trigrams$word))
        #print(searchString)
        #print(nrow(trigramWords_table))
        #print(trigramWords_table)
        
        ngram.table <- trigramWords_table
        
        # ngram.table <- trigrams
        obs.sum <- nrow(ngram.table)
        #freq.sum <- sum(ngram.table$frequency)
        freq.sum <- sum(trigrams$frequency)
        
        ## find all unique frequencies
        found.freq <- sort(unique(ngram.table$frequency), decreasing = FALSE)
        
        ## find frequency of frequency for eachh of these frequencies
        fof <- NULL
        Nth.term <- NULL
        Nth.freq <- NULL
        for (i in 1:length(found.freq)){
                freq.table <- data.frame(
                        Nth.term <- c(Nth.term, paste("N", found.freq[i], sep = "")),
                        Nth.freq <- c(Nth.freq, found.freq[i]),
                        fof <- c(fof, sum(ngram.table$frequency == found.freq[i]))
                )
        }
        colnames(freq.table) <- c("Nth.term", "freq", "fof")
        #print(freq.table)
        
        ##Find probability of unseen word
        Pgt.unseen <- NULL
        for (i in 1:nrow(freq.table)){
                Pgt.unseen <- c(Pgt.unseen, fof[1]/freq.sum)
        }        
        freq.table$Pgt.unseen <- Pgt.unseen
        #print(freq.table)
        
        ##Find probability of seen words
        Pgt <- NULL
        for (i in 1:nrow(freq.table)){
                #print(paste("i:", i, sep = " "))
                #Good Turing requires you to take N + 1; however, we may NOT have next N depending on
                #frequency of the word.  So, instead of taking next N for next frequency, I choose
                #to take next N for next available frequency.  As far as last N is concern, it is very
                #likely that the fof would be 1 so I choose to multiply by 1
                if (i != nrow(freq.table)){
                        #print(paste("Next N:", freq.table$fof[i+1], sep = " "))
                        #print(paste("This N:", freq.table$fof[i], sep = " "))
                        c.star <- (freq.table$freq[i] + 1) * (freq.table$fof[i+1]/freq.table$fof[i])
                        Pgt <- c(Pgt, (c.star*(1/freq.sum)))
                } else {
                        c.star <- (freq.table$freq[i]) * (freq.table$fof[i]/freq.table$fof[i])
                        Pgt <- c(Pgt, (c.star*(1/freq.sum)))
                }
        }        
        freq.table$Pgt <- Pgt
        #print(freq.table)
        
        ## Add two new columns in trigramWords_table
        trigramWords_table["Pgt.unseen"] <- NA
        trigramWords_table["Pgt"] <- NA
        
        ## Now let's assign probability to each words
        for (i in 1:nrow(trigramWords_table)){
                this.freq <- trigramWords_table$frequency[i]
                searchPhrase <- paste0('^\\<', this.freq, '$')
                this.row <- subset(freq.table, grepl(searchPhrase, freq.table$freq))
                #Add Pgt.unseen and Pgt to trigramWords_table
                trigramWords_table$Pgt.unseen[i] <- this.row$Pgt.unseen
                trigramWords_table$Pgt[i] <- this.row$Pgt
        }
        ##Arrange in decending order
        trigramWords_table <- trigramWords_table[order(-trigramWords_table$Pgt),]

        #Remove the searchString from the word list to provide next word only instead of trigram
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        trigramWords_table$word <- gsub(searchPhrase, "", paste(trigramWords_table$word))        
        nextWords <- as.character(head(trigramWords_table$word, 5))
        #print(nextWords)        
        return(nextWords)
}

