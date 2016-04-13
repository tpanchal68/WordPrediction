t <- proc.time()
# **Load the R package for text mining and then load your texts into R.**
library(tm)
library(textmineR)
library(R.utils)
library(stringi)
library(SnowballC)
library(ggplot2)
library(wordcloud)

##########################################################################################
#                                  Loading Texts                                         #
########################################################################################## 
cname <- file.path("..", "Data", "Coursera-SwiftKey", "final", "en_US")   
cname 
dir(cname)
filenames <- DirSource(cname)$filelist
print(paste("Files Detected:", length(filenames)))
print(filenames)
LinesToRead <- 5000
combined_data <- NULL
nlines <- NULL
fsizes <- NULL
nwords <- NULL
filetype <- NULL

for (filename in filenames){
        if (grepl("blogs", filename)){
                filetype <- c(filetype, "blogs")
        }
        if (grepl("news", filename)){
                filetype <- c(filetype, "news")
        }
        if (grepl("twitter", filename)){
                filetype <- c(filetype, "twitter")
        }
        fsizes <- c(fsizes, file.info(filename)$size/1024^2)
        nlines <- c(nlines, countLines(file = paste(filename, sep = "")))
        thisDoc <- list(paste(readLines(file(filename), nlines, encoding = "UTF-8", warn = FALSE), collapse="\n") )
        nwords <- c(nwords, sum(stri_count_words(thisDoc)))
        close(file(filename))
}
rm(thisDoc)
data_summary <- data.frame(
        filetype, fsizes, nlines, nwords
)
colnames(data_summary) <- c("Filetype", "Filesize_MB", "Number_of_lines", "Number_of_words")
print(data_summary)

p1 <- ggplot(data = data_summary, aes(x=Filetype, y=Filesize_MB)) +
        geom_bar(stat = "identity") + coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("Data Type") + ylab("Filesize in MB") +
        labs(title = "Filesize Plot")
print(p1)

p2 <- ggplot(data = data_summary, aes(x=Filetype, y=Number_of_lines)) +
        geom_bar(stat = "identity") + coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("Data Type") + ylab("Number of Lines") +
        labs(title = "Lines per file Plot")
print(p2)

p3 <- ggplot(data = data_summary, aes(x=Filetype, y=Number_of_words)) +
        geom_bar(stat = "identity") + coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("Data Type") + ylab("Number of Words") +
        labs(title = "Words per file Plot")
print(p3)

for (filename in filenames){
        combined_data <- c(combined_data,list(paste(readLines(file(filename), LinesToRead), collapse="\n") ));
}

##########################################################################################
#                                Start Your Analysis                                     #
##########################################################################################
#Create Corpus
docs <- Corpus(VectorSource(combined_data))
getTransformations()

##create the toSpace content transformer to eliminate unwanted characters
removeSpecialChars <- content_transformer(function (x, pattern) gsub(pattern, "", x))
docs <- tm_map(docs, removeSpecialChars, "[^a-zA-Z0-9 ]") #remove everything except alphanumeric and space
docs <- tm_map(docs, removeSpecialChars, "http[[:alnum:]]*") #remove URL
docs <- tm_map(docs, removePunctuation)  #Remove punctuation
docs <- tm_map(docs, removeNumbers) #Strip digits
docs <- tm_map(docs, content_transformer(tolower)) #Transform to lower case
docs <- tm_map(docs, removeWords, c("fuck", "bitch", "ass", "cunt", "pussy", "asshole")) #Remove bad words
docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords

#Keep a copy of corpus to use later as a dictionary for stem completion
mydocsCopy <- docs
docs <- tm_map(docs, stemDocument)   #Stem document
#Define modified stemCompletion function
stemCompletion_mod <- function(x,dict=mydocsCopy) {
        PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
}
docs <- tm_map(docs, stripWhitespace) #Strip whitespace
docs <- tm_map(docs, PlainTextDocument) #Convert to plaintextdocument to complete preprocessing

##Tokenize corpus into ngram with RWeka
# library(RWeka)
# myTokenizer <- function(x) {
#         RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))
# }

##Tokenize corpus into ngram with textmineR
myTokenizer <- NgramTokenizer(min=1, max=4)
docs_tdm <- TermDocumentMatrix(docs, control = list(tokenize=myTokenizer))

#inspect(docs_tdm)
docs_tdm_matrix <- removeSparseTerms(docs_tdm, 0.99) # This makes a matrix that is 99% empty space, maximum.   
print(docs_tdm_matrix)
freqTerms <- findFreqTerms(docs_tdm_matrix, lowfreq=5)   # Change number to whatever is most appropriate for your text data.
termFrequency <- rowSums(as.matrix(docs_tdm_matrix[freqTerms,]))
termFrequency <- data.frame(word=names(termFrequency), frequency=termFrequency)
#Arrange in Ascending order
ordered_termFrequency <- termFrequency[with(termFrequency, order(-frequency)), ]
Top20 <- ordered_termFrequency[1:20, ]
print(Top20)

#ngram Data Plot
g <- ggplot(Top20, aes(x=word, y=frequency)) +
        geom_bar(stat = "identity") +  coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("Words") + ylab("Frequency") +
        labs(title = "Frequent words")
print(g)

#Unigram Word Clouds!
dark2 <- brewer.pal(6, "Dark2")
wordcloud(
        termFrequency$word,
        termFrequency$frequency,
        scale=c(8,.9), 
        min.freq=3,
        max.words=300, 
        random.order=TRUE, 
        rot.per=0.10, 
        colors=dark2
)

write.csv(ordered_termFrequency, file = "ordered_termFrequency.csv", row.names = FALSE, na="")
#print(findAssocs(docs_tdm, c("birthday", "love"), 0.99))
print(proc.time() - t)
