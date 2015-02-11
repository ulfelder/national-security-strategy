rm(list=ls(all=TRUE))
setwd("c:/users/jay/documents/national-security-strategy")

library(tm)
library(SnowballC)

## PREPROCESSING ##

# Read all texts into a single corpus; unsure of encoding here...
NSS <- VCorpus(DirSource("C:/Users/Jay/Documents/national-security-strategy/texts", encoding = "utf-8"))

# Create matrix of term counts by document
NSS.tdm <- TermDocumentMatrix(NSS, control = list(stripWhitespace = TRUE, 
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE, 
                                                  stopwords = TRUE, 
                                                  tolower = TRUE,
                                                  stemDocument = TRUE
))



# Most of the code below is modified from chapter 3 of "Machine Learning for Hackers"
# Buy the book here: http://shop.oreilly.com/product/0636920018483.do

# Convert tdm to matrix
NSS.matrix <- as.matrix(NSS.tdm)

# Count occurance of each term
NSS.counts <- rowSums(NSS.matrix)

# Create dataframe of terms and count data 
NSS.df <- data.frame(cbind(names(NSS.counts),
                           as.numeric(NSS.counts)), stringsAsFactors = FALSE)

# Rename columns
names(NSS.df) <- c("term", "frequency")

# Convert frequency to numeric
NSS.df$frequency <- as.numeric(NSS.df$frequency)

# Calculate the % of documents containing a term
NSS.occurence <- sapply(1:nrow(NSS.matrix),
                        function(i){length(which(NSS.matrix[i,]>0))/ncol(NSS.matrix)})
# Caclulate the % of times a term appears in all documents
NSS.density <- NSS.df$frequency/sum(NSS.df$frequency)

# Merge data into one dataframe
NSS.df <- transform(NSS.df, density = NSS.density, occurence = NSS.occurence)