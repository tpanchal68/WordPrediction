library(data.table)

# Read data file and load data
if(!exists("unigrams")){
        unigrams <- readRDS("unigrams.rds")
        unigrams$word <- as.character(unigrams$word)
}
if(!exists("bigrams")){
        bigrams <- readRDS("bigrams.rds")
        bigrams$word <- as.character(bigrams$word)
}
if(!exists("trigrams")){
        trigrams <- readRDS("trigrams.rds")
        trigrams$word <- as.character(trigrams$word)
}
