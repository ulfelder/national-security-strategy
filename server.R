# Load required packages
library(tm)
library(SnowballC)
library(wordcloud)
library(Hmisc)
library(topicmodels)
library(RColorBrewer)

# Read all texts into a single corpus; unsure of encoding here...
NSS <- VCorpus(DirSource("texts/", encoding = "utf-8"))

# Perform various preprocessing steps
NSS <- tm_map(NSS, stripWhitespace)
NSS <- tm_map(NSS, removeNumbers)
NSS <- tm_map(NSS, removePunctuation)
NSS <- tm_map(NSS, content_transformer(tolower)) # Do before stopword removal to get sentence starts
NSS <- tm_map(NSS, removeWords, stopwords("english"))
NSS.stemmed <- tm_map(NSS, stemDocument) # Keep original for word clouds

# Create matrix of stemmed term counts by document for associations
NSS.tdm <- TermDocumentMatrix(NSS.stemmed)

# Load version with stems completed for line plots
NSS.df <- read.csv("data/nss.tdm.csv", stringsAsFactors = FALSE)

# Function for line plot of word count over time
plotWF <- function(word, normalize) {
  
  # Convert to lower case just in case the user doesn't remember to do this.
  t <- tolower(word)

  # Subset to rows for terms that contain 'word' & columns with the counts
  z <- NSS.df[grep(t, NSS.df[,1]),2:17]

  # Sum counts across rows and normalize by total words in each report if 
  # desired
  if (normalize) {
    s <- (colSums(z)/colSums(NSS.df[,2:17])) * 100
  } else {
    s <- colSums(z)
  }

  # Get vector of years for labeling x-axis
  yrs <- substr(names(NSS.df), 5, 8)[2:17]

  # Make the plot
  par(cex.axis = 1, cex.main = 1.5)
  plot(x = 1:length(yrs), y = s, type = "l", lwd = 2, col = "firebrick2",
       xlab = "", ylab = "", axes = FALSE,
       main = t)
  axis(1, at = 1:length(yrs), labels = yrs, tick = FALSE, las = 2)
  axis(2, tick = FALSE, las = 2)
  abline(h = 0, lwd = 0.5, col = "gray")

}

# Function for word cloud of 50 most prevalent words in unstemmed corpus
wordle <- function(doc) {
  
  require(wordcloud)
  
  wordcloud(NSS[as.numeric(doc)],
            scale = c(5,0.5),
            max.words = 50,
            random.order = FALSE,
            rot.per = 0.1,
            use.r.layout = FALSE,
            colors = brewer.pal(8, "Dark2"))
}

# Function to make dot plot of top associations
termcorr <- function(term2) {
  
  require(Hmisc)
  
  # Get the vector of correlation coefficients
  z <- findAssocs(NSS.tdm, term2, 0.5)
  
  # Make a dot plot of the results, limited to top 10
  dotchart2(z[1:10],
            labels = dimnames(z)[[1]][1:10],
            lines = TRUE, lwd = 0.05, lty = 3,
            sort = FALSE,
            dotsize = 1.25, pch = 20, col = "hotpink3",
            cex.labels = 1,
            xlim = c(0.5,1))
  title(main = list(term2, cex = 1.25)) 
  
}

## MAKE STUFF ##

shinyServer(function(input, output) {

  library(wordcloud)
  library(Hmisc)
  library(RColorBrewer)
  
  output$lineplot <- renderPlot({
    
    plotWF(input$term, input$normalize)
    
  })
  
  output$cloud <- renderPlot({
    
    wordle(input$report)
    
  })
  
  output$dotplot <- renderPlot({
    
    termcorr(input$term2)
    
  })
  
})