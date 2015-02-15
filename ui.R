shinyUI(fluidPage(
  titlePanel("Exploring the U.S. National Security Strategy Reports"),
  
  sidebarLayout(position = "right",
    
    sidebarPanel(textInput("term", h4("Input a Term"), value = "terror")),
    
    mainPanel(
      p("This app lets you explore change over time in the national security concerns of the U.S.
        government by plotting normalized term counts from all sixteen U.S. National Security Strategy
        Reports, published irregularly by the White House from 1987 to 2015. The motivating assumption
        is that a term's prevalence in the report gives some indication of the salience of the entity or
        idea that term represents to the president's view of U.S. national security."),
      p("Use the box on the right to input a term to plot. The plotting function will find all words
        in the data set (a term-document matrix) that include that term; sum the counts of those words'
        appearances by report; divide those sums by the total number of words in each report, and multiply
        the result by 100 so it can be read as a percentage. For example, inputting 'afghan' will sum
        counts by report for the terms 'afghan', 'afghans', afghanistan', and 'afghanistans'. The function
        is not case sensitive."),
      p("The data and scripts used to build this app can be found on GitHub, ",
        a(href = "https://github.com/ulfelder/national-security-strategy", "here"),
      ". This app was built by ",
        a(href = "https://dartthrowingchimp.wordpress.com/", "Jay Ulfelder"),
      "."),
      plotOutput("lineplot")
    )
    
)))