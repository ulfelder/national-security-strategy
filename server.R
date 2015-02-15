NSS.tdm <- read.csv("data/nss.tdm.csv")

plotWF <- function(word) {
  t <- tolower(word)  # Make lower case just in case the user doesn't remember to do this.
  z <- NSS.tdm[grep(t, NSS.tdm[,1]),2:17]  # Subset to rows for terms that contain 'word' & data columns
  s <- colSums(z)  # Get sums by document (column) across those rows
  yrs <- substr(names(NSS.tdm), 5, 8)[2:17]  # Get vector of years for labeling x-axis
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