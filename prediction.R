library(doParallel)

#Get hint to complete this word
hintThisWord <- function(searchString){
        hintWords <- NULL
        
        searchPhrase <- paste0('^\\<', searchString)
        hintWords_table <- subset(unigrams, grepl(searchPhrase, unigrams$word))
        hintWords <- as.character(head(hintWords_table$word, 5))
        
        return(hintWords)
}

#MLE smoothing prediction
getMlePrediction <- function(searchString, ngramtable){
        #Let's first find all possible ngrams based on searchString from given ngram table
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        # mleWord_table <- subset(trigrams, grepl(searchPhrase, trigrams$word))
        mleWord_table <- subset(ngramtable, grepl(searchPhrase, ngramtable$word))
        
        #Now let's calculate probability of each ngrams
        probability <- NULL
        for (i in 1:nrow(mleWord_table)){
                thisWordFreq <- mleWord_table$frequency[i]
                #print(mleWord_table$word[i])
                #Pml(Wi|Wi-2Wi-1) = Count(Wi-2Wi-1Wi)/Count(Wi-2Wi-1)
                #Probability of this word given previous two words = count of this word/count of previous two words
                probability <- c(probability, (thisWordFreq/sum(mleWord_table$frequency)))
        }
        #Now append probability column to Words_table
        mleWord_table$probability <- probability
        #print(head(mleWord_table))
        
        #Remove the searchString from the word list
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        mleWord_table$word <- gsub(searchPhrase, "", paste(mleWord_table$word))
        nextWords <- as.character(head(mleWord_table$word, 5))
        
        return(nextWords)
}

#Laplace smoothing prediction
getLaplacePrediction <- function(searchString, ngramtable){
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        laplaceWord_table <- subset(ngramtable, grepl(searchPhrase, ngramtable$word))
        
        #Now let's calculate probability of each trigrams
        probability <- NULL
        for (i in 1:nrow(laplaceWord_table)){
                thisWordFreq <- laplaceWord_table$frequency[i]
                probability <- c(probability, ((thisWordFreq + 1)/(sum(laplaceWord_table$frequency) + nrow(trigrams))))
        }
        #Now append probability column to ngramWords_table
        laplaceWord_table$probability <- probability
        #print(head(laplaceWord_table))
        
        #Remove the searchString from the word list to provide next word only instead of trigram
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        laplaceWord_table$word <- gsub(searchPhrase, "", paste(laplaceWord_table$word))
        nextWords <- as.character(head(laplaceWord_table$word, 5))
        
        return(nextWords)
}

#Interpolation smoothing prediction
getInterPollationPrediction <- function(searchString, ngramtable){
        #Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        ngramWords_table <- subset(ngramtable, grepl(searchPhrase, ngramtable$word))
        #View(ngramWords_table)
        #print(nrow(ngramWords_table))
        
        #Now let's calculate probability of each trigrams
        #myEM_trigram <- normalmixEM(trigrams$frequency, mu = c(0,1), sigma=c(1,1), sd.constr=c(1,1) )
        #myEM_trigram$lambda
        if (length(unlist(strsplit(searchPhrase, " "))) > 1){
                trigram.Prob.Calc <- "TRUE"
                lambda1 <- 0.7
                lambda2 <- 0.29
                lambda3 <- 0.01
        } else {
                trigram.Prob.Calc <- "FALSE"
                lambda2 <- 0.95
                lambda3 <- 0.05
                bigramWords_table <- ngramWords_table
        }

        ##Start parallel process
        registerDoSEQ()
        # Detect the CPU cores
        cl <- makeCluster(detectCores(), type='PSOCK')
        registerDoParallel(cl)
        set.seed(1)

        if (trigram.Prob.Calc == "TRUE"){
                probability <- NULL
                for (i in 1:nrow(ngramWords_table)){
                        thisWordFreq <- ngramWords_table$frequency[i]
                        probability <- c(probability, (lambda1 * (thisWordFreq/sum(ngramWords_table$frequency))))
                }
                #Now append probability column to ngramWords_table
                ngramWords_table$probability <- probability
                #View(ngramWords_table)
                
                #Now let's find all possible bigrams based on the last word in searchString
                bigramWord <- NULL
                for (i in 1:nrow(ngramWords_table)){
                        splittedWords <- unlist(strsplit(as.character(ngramWords_table$word[i]), " "))
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
        }
        
        #Now let's calculate probability of each bigrams
        probability <- NULL
        for (i in 1:nrow(bigramWords_table)){
                thisWordFreq <- bigramWords_table$frequency[i]
                probability <- c(probability, (lambda2 * (thisWordFreq/sum(bigramWords_table$frequency))))
        }
        #Now append probability column to bigramWords_table
        bigramWords_table$probability <- probability
        #View(bigramWords_table)
        
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
        probability <- NULL
        for (i in 1:nrow(unigramWords_table)){
                thisWordFreq <- unigramWords_table$frequency[i]
                #probability <- c(probability, (lambda3 * (thisWordFreq/sum(unigramWords_table$frequency))))
                #probability <- c(probability, (lambda3 * (thisWordFreq/sum(unigrams$frequency))))
                probability <- c(probability, (lambda3 * (thisWordFreq/nrow(unigrams))))
        }
        #Now append probability column to unigramWords_table
        unigramWords_table$probability <- probability
        #View(unigramWords_table)
        
        #Now let's find each unigram from bigram and trigram and extract it's probability
        cumulative_prob <- NULL
        for (i in 1:nrow(unigramWords_table)){
                searchPhrase <- paste0('\\<', as.character(unigramWords_table$word[i]), '$')
                if (trigram.Prob.Calc == "TRUE"){
                        tri_row <- subset(ngramWords_table, grepl(searchPhrase, ngramWords_table$word))
                }
                bi_row <- subset(bigramWords_table, grepl(searchPhrase, bigramWords_table$word))
                uni_row <- subset(unigramWords_table, grepl(searchPhrase, unigramWords_table$word))
                if (trigram.Prob.Calc == "TRUE"){
                        cumulative_prob <- c(cumulative_prob, (tri_row$probability + bi_row$probability + uni_row$probability))
                } else {
                        cumulative_prob <- c(cumulative_prob, (bi_row$probability + uni_row$probability))
                }
        }
        ##Stop parallel process
        stopCluster(cl)
        
        #Append cumulative_prob column to unigramWords_table
        unigramWords_table$cumulative_prob <- cumulative_prob
        unigramWords_table <- unigramWords_table[order(-unigramWords_table$cumulative_prob),]
        #saveRDS(unigramWords_table, file="unigramWords_table.rds")
        
        #View(unigramWords_table)
        nextWords <- as.character(head(unigramWords_table$word, 5))
        #print(nextWords)
        return(nextWords)
}

#Good Turing word prediction
getGoodTuringPrediction <- function(searchString, ngramtable){
        ##Let's first find all possible trigrams based on searchString
        searchPhrase <- paste0('^\\<', searchString, '\\>')
        ngramWords_table <- subset(ngramtable, grepl(searchPhrase, ngramtable$word))
        #print(searchString)
        #print(nrow(ngramWords_table))
        #print(ngramWords_table)
        
        ngram.temp.table <- ngramWords_table
        
        # ngram.temp.table <- trigrams
        obs.sum <- nrow(ngram.temp.table)
        #freq.sum <- sum(ngram.temp.table$frequency)
        freq.sum <- sum(trigrams$frequency)
        
        ## find all unique frequencies
        found.freq <- sort(unique(ngram.temp.table$frequency), decreasing = FALSE)
        
        ## find frequency of frequency for eachh of these frequencies
        fof <- NULL
        Nth.term <- NULL
        Nth.freq <- NULL
        for (i in 1:length(found.freq)){
                freq.table <- data.frame(
                        Nth.term <- c(Nth.term, paste("N", found.freq[i], sep = "")),
                        Nth.freq <- c(Nth.freq, found.freq[i]),
                        fof <- c(fof, sum(ngram.temp.table$frequency == found.freq[i]))
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
        
        ## Add two new columns in ngramWords_table
        ngramWords_table["Pgt.unseen"] <- NA
        ngramWords_table["Pgt"] <- NA
        
        ## Now let's assign probability to each words
        for (i in 1:nrow(ngramWords_table)){
                this.freq <- ngramWords_table$frequency[i]
                searchPhrase <- paste0('^\\<', this.freq, '$')
                this.row <- subset(freq.table, grepl(searchPhrase, freq.table$freq))
                #Add Pgt.unseen and Pgt to ngramWords_table
                ngramWords_table$Pgt.unseen[i] <- this.row$Pgt.unseen
                ngramWords_table$Pgt[i] <- this.row$Pgt
        }
        ##Arrange in decending order
        ngramWords_table <- ngramWords_table[order(-ngramWords_table$Pgt),]

        #Remove the searchString from the word list to provide next word only instead of trigram
        searchPhrase <- paste0('^\\<', searchString, '\\s+')
        ngramWords_table$word <- gsub(searchPhrase, "", paste(ngramWords_table$word))        
        nextWords <- as.character(head(ngramWords_table$word, 5))
        #print(nextWords)        
        return(nextWords)
}

