rm(list=ls(all=TRUE))
setwd("c:/users/jay/documents/national-security-strategy")

library(tm)
library(SnowballC)
library(topicmodels)

# Read all texts into a single corpus; unsure of encoding here...
NSS <- VCorpus(DirSource("C:/Users/Jay/Documents/national-security-strategy/texts", encoding = "utf-8"))

## WORD COUNTS OVER TIME ##

# Create matrix of term counts by document
NSS.tdm <- TermDocumentMatrix(NSS, control = list(stripWhitespace = TRUE, 
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE, 
                                                  stopwords = TRUE, 
                                                  tolower = TRUE,
                                                  stemDocument = TRUE,
                                                  minWordLength = 4))

# Convert from list to matrix
NSS.matrix <- as.matrix(NSS.tdm)

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

# Make a function to generate a line plot of the frequency of a particular word over time.
plotWF <- function(word) {
    yrs <- substr(names(NSS.df), 5, 8)[2:17]
    plot(x = 1:length(yrs), y = subset(NSS.df, term==word, select = c(2:17)),
        type = "l", lwd = 2, xlab = "", ylab = "", axes = FALSE, main = word)
    axis(1, at = 1:length(yrs), labels = yrs, tick = FALSE, las = 2)
    axis(2, tick = FALSE, las = 2)
}

plotWF("soviet")
plotWF("china")
plotWF("iraq")
plotWF("afghanistan")
plotWF("iran")
plotWF("alqaida")
plotWF("cyber")
plotWF("nuclear")
plotWF("terrorism")
plotWF("terrorists")  # Shows that stemming isn't working as hoped. Hmmm...

## TOPIC MODELING ##

# Following vignette for 'topicmodels'
# http://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

# Create document term matrix (not to be confused with term document matrix)
NSS.dtm <- DocumentTermMatrix(NSS, control = list(stripWhitespace = TRUE, 
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE, 
                                                  stopwords = TRUE, 
                                                  tolower = TRUE,
                                                  stemDocument = TRUE,
                                                  minWordLength = 3))

# Use median tf-idf as threshold to omit terms that are super-rare or common across
# documents (so, little discriminatory power)
library("slam")
term_tfidf <- tapply(NSS.dtm$v/row_sums(NSS.dtm)[NSS.dtm$i], NSS.dtm$j, mean) *
    log2(nDocs(NSS.dtm)/col_sums(NSS.dtm > 0))
NSS.dtm.sub <- NSS.dtm[,term_tfidf >= median(term_tfidf)]
NSS.dtm.sub <- NSS.dtm.sub[row_sums(NSS.dtm.sub) > 0,]
dim(NSS.dtm.sub)

# Now do the topic modeling
k <- 10  # Set number of topics to 10 (30 was way too many)
SEED <- 709  # Set seed to make it replicable
NSS_TM <- list(VEM = LDA(NSS.dtm.sub, k = k, control = list(seed = SEED)),
    VEM_fixed = LDA(NSS.dtm.sub, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
    Gibbs = LDA(NSS.dtm.sub, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
    CTM = CTM(NSS.dtm.sub, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))

# "To compare the fitted models we first investigate the alpha values of the models # fitted with VEM and alpha estimated and with VEM and alpha fixed."
sapply(NSS_TM[1:2], slot, "alpha")
# "We see that if alpha is estimated it is set to a value much smaller than the default. This 
# indicates that in this case the Dirichlet distribution has more mass at the corners and hence,
# documents consist of only a few topics."

# "The entropy measure can also be used to indicate how the topic distributions differ for the four
# fitting methods. We determine the mean entropy for each fitted model over the documents.
# The term distribution for each topic as well as the predictive distribution of topics for a
# document can be obtained with posterior(). A list with components "terms" for the term
# distribution over topics and "topics" for the topic distributions over documents is returned.
# Higher values indicate that the topic distributions are more evenly spread over the topics."
sapply(NSS_TM, function(x)
    mean(apply(posterior(x)$topics,
        1, function(z) - sum(z * log(z)))))

# Get a matrix with the k (here, 5) most likely topics for each document from a particular
# fitting method (here, Gibbs and VEM)
Topics.Gibbs <- topics(NSS_TM[["Gibbs"]], k = 5)
Topics.VEM <- topics(NSS_TM[["VEM"]], k = 5)

# List the k (here, 10) most frequent terms for each topic from a particular fitting method.
Terms.Gibbs <- terms(NSS_TM[["Gibbs"]], 10)
Terms.VEM <- terms(NSS_TM[["VEM"]], 10)

# Inspect terms for topic identified as most likely in a few reports. Here, the first number
# in the brackets identifies the topic by likelihood, and the second number identifies the 
# report by its position in the sequence.
Terms.VEM[,Topics.VEM[1,16]]
Terms.VEM[,Topics.VEM[2,16]]
Terms.VEM[,Topics.VEM[1,15]]
Terms.VEM[,Topics.VEM[2,15]]
Terms.VEM[,Topics.VEM[1,1]]

