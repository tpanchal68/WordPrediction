t <- proc.time()
# **Load the R package for text mining and then load your texts into R.**
source("textCleaner.R")
library(tm)
library(RWeka)
library(R.utils)
library(stringi)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)   
library(fpc)   
library(doParallel)

##########################################################################################
#                                  Loading Texts                                         #
########################################################################################## 
cname <- file.path("..", "Data", "Coursera-SwiftKey", "final", "en_US")   
cname 
dir(cname)
filenames <- DirSource(cname)$filelist
print(paste("Files Detected:", length(filenames)))
print(filenames)
LinesToRead <- 40000
combined_data <- NULL
cleaned_text <- NULL

for (filename in filenames){
        combined_data <- c(combined_data, list(paste(readLines(file(filename), LinesToRead, encoding = "UTF-8", warn = FALSE), collapse="\n")))
        close(file(filename))
}

for (i in 1:length(filenames)){
        cleaned_text <- c(cleaned_text, list(TextCleaner(paste(combined_data[[i]], collapse = " "))))
}
#print(cleaned_text)

#Load profanity
profanity <- readRDS("profanity.rds")
#Load apostrophe Words
apostropheWords <- readRDS("apostropheWords.rds")
#Load singlechars Words
singlechars <- readRDS("singlechars.rds")

rm(combined_data)
gc() #- do garbage collection now

##########################################################################################
#                                Start Your Analysis                                     #
##########################################################################################
#Create Corpus
docs <- Corpus(VectorSource(cleaned_text))
getTransformations()

##create the toSpace content transformer to eliminate unwanted characters
toSpace <- content_transformer(function(x, pattern) (return (gsub(pattern, " ", x))))
docs <- tm_map(docs, toSpace, "[^A-Za-z]+")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, profanity) #Remove profanity
docs <- tm_map(docs, removeWords, apostropheWords) #Remove apostropheWords word
docs <- tm_map(docs, removeWords, singlechars) #Remove single chars

#docs <- tm_map(docs, removeWords, stopwords("english")) #remove stopwords
#Keep a copy of corpus to use later as a dictionary for stem completion
# mydocsCopy <- docs
# docs <- tm_map(docs, stemDocument)   #Stem document
# #Define modified stemCompletion function
# stemCompletion_mod <- function(x,dict=mydocsCopy) {
#         PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
# }
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument) #Convert to plaintextdocument to complete preprocessing

##Start parallel process
registerDoSEQ()
# Detect the CPU cores
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
set.seed(1)

print("Creating TDM matrix and generating N-grams...")
##Tokenize corpus into ngram with RWeka
TrigramTokenizer <- function(x) {
        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))
}

docs.tdm <- TermDocumentMatrix(docs, control = list(wordLengths=c(1,Inf), tokenize = TrigramTokenizer))
#inspect(docs.tdm)
#inspect(docs.tdm[1:10, 1:3])

docs.tdm <- removeSparseTerms(docs.tdm, 0.99) # This makes a matrix that is 99% empty space, maximum.   
print(docs.tdm)
freqTerms <- findFreqTerms(docs.tdm, lowfreq=1)
termFrequency <- rowSums(as.matrix(docs.tdm[freqTerms,]))
termFrequency <- data.frame(word=names(termFrequency), frequency=termFrequency)
#Arrange in Ascending order
termFrequency <- termFrequency[with(termFrequency, order(-frequency)), ]
row.names(termFrequency) <- NULL
Top20 <- termFrequency[1:20, ]
#print(Top20)
#View(termFrequency)
#saveRDS(termFrequency, file="ngrams.rds")

##Split termFrequency table into unigrams, bigrams, and trigrams
unigrams <- termFrequency[which(sapply(gregexpr("\\S+", termFrequency$word), length) == 1), ]
saveRDS(unigrams, file="unigrams.rds")

bigrams <- termFrequency[which(sapply(gregexpr("\\S+", termFrequency$word), length) == 2), ]
saveRDS(bigrams, file="bigrams.rds")

trigrams <- termFrequency[which(sapply(gregexpr("\\S+", termFrequency$word), length) == 3), ]
saveRDS(trigrams, file="trigrams.rds")

##Stop parallel process
stopCluster(cl)

# #####ngram Data Plot
# g <- ggplot(Top20, aes(x=word, y=frequency)) +
#         geom_bar(stat = "identity") +  coord_flip() +
#         theme(legend.title=element_blank()) +
#         xlab("Words") + ylab("Frequency") +
#         labs(title = "Frequent words")
# print(g)
# 
# #####ngram Word Clouds!
# dark2 <- brewer.pal(6, "Dark2")
# wordcloud(
#         termFrequency$word,
#         termFrequency$frequency,
#         scale=c(8,.9), 
#         min.freq=3,
#         max.words=300, 
#         random.order=TRUE, 
#         rot.per=0.10, 
#         colors=dark2
# )
# 
# d <- dist(t(docs.tdm), method="euclidian")   
# fit <- hclust(d=d, method="ward")   
# fit 
# plot.new()
# plot(fit, hang=-1)
# groups <- cutree(fit, k=1:3)   # "k=" defines the number of clusters you are using   
# rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around the 5 clusters   
# 
# #####K-means clustering
# d <- dist(t(docs.tdm), method="euclidian")   
# kfit <- kmeans(d, 2)   
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

#print(findAssocs(docs.tdm, c("birthday", "love"), 0.99))
print(proc.time() - t)
