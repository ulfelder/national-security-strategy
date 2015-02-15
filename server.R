NSS.tdm <- read.csv("data/nss.tdm.csv")

plotWF <- function(word) {
  
  # Convert to lower case just in case the user doesn't remember to do this.
  t <- tolower(word)

  # Subset to rows for terms that contain 'word' & columns with the counts
  z <- NSS.tdm[grep(t, NSS.tdm[,1]),2:17]

  # Sum counts across rows and normalize by total words in each report
  s <- (colSums(z)/colSums(NSS.tdm[,2:17])) * 100

  # Get vector of years for labeling x-axis
  yrs <- substr(names(NSS.tdm), 5, 8)[2:17]

  # Make the plot
  par(cex.axis = 0.9)
  plot(x = 1:length(yrs), y = s, type = "l", lwd = 2, col = "firebrick2",
       xlab = "", ylab = "", axes = FALSE,
       main = t)
  axis(1, at = 1:length(yrs), labels = yrs, tick = FALSE, las = 2)
  axis(2, tick = FALSE, las = 2)
  abline(h = 0, lwd = 0.5, col = "gray")

}

shinyServer(function(input, output) {
    
  output$lineplot <- renderPlot({
    
    plotWF(input$term)
    
  })
  
})