rm(list=ls(all=TRUE))
setwd("c:/users/jay/documents/national-security-strategy")

library(tm)
library(SnowballC)

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

# Convert from list to matrix
NSS.matrix <- as.matrix(NSS.tdm)

# Make that a data frame with the terms as the first column
NSS.df <- as.data.frame(cbind(row.names(NSS.matrix), NSS.matrix), stringsAsFactors = FALSE)

# Give the columns in that data frame better names
names(NSS.df) <- c("term", paste0("nss.", substr(list.files("texts/"), 1, 4)))

# Fix the count columns to be read as numbers
for (i in (2:length(names(NSS.df)))){ NSS.df[,i] <- as.numeric(NSS.df[,i]) }

# Get rid of the row names
row.names(NSS.df) <- NULL

