rm(list=ls(all=TRUE))
setwd("c:/users/jay/documents/national-security-strategy")

library(tm)
library(SnowballC)

## PREPROCESSING ##

# Read all texts into a single corpus; unsure of encoding here...
NSS <- VCorpus(DirSource("C:/Users/Jay/Documents/national-security-strategy/texts", encoding = "utf-8"))

# Standard preprocessing steps
NSS <- tm_map(NSS, stripWhitespace)
NSS <- tm_map(NSS, removePunctuation)
NSS <- tm_map(NSS, removeNumbers)
NSS <- tm_map(NSS, tolower)
NSS <- tm_map(NSS, removeWords, stopwords("en"))
NSS <- tm_map(NSS, stemDocument)

## APPLICATIONS ##

# Create matrix of term counts by document
# NOTE: not working -- I get Error: inherits(doc, "TextDocument") is not TRUE
NSS.dtm <- DocumentTermMatrix(NSS)