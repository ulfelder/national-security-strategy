shinyUI(fluidPage(
  titlePanel("Exploring the U.S. National Security Strategy Reports"),
  
  sidebarLayout(position = "right",
    
    sidebarPanel(
      textInput("term", h4("Input a Term"), value = "terror"),
      checkboxInput("normalize", 
                    label="Normalize by total words in story", value=TRUE)
      ),
    
    mainPanel(
      p("This app lets you explore change over time in the national security concerns of the U.S.
        government by plotting term counts from all 16  U.S. National Security Strategy Reports,
        published irregularly by the White House from 1987 to 2015. The motivating assumption
        is that a term's prevalence in the report gives some indication of the salience of the entity or
        idea that term represents to the president's view of U.S. national security."),
      p("Use the text field on the right to input a term to plot, and check or uncheck the box to switch between normalized
        and raw counts. The plotting function will find all words in the data set (a term-document matrix) that include that
        term you type. For example, inputting 'afghan' will combine counts for the terms 'afghan', 'afghans', afghanistan',
        and 'afghanistans'. The function is not case sensitive."),
      p("The data and scripts used to build this app can be found on GitHub, ",
        a(href = "https://github.com/ulfelder/national-security-strategy", "here"),
      ". This app was built by ",
        a(href = "https://dartthrowingchimp.wordpress.com/", "Jay Ulfelder"),
      ". ",
        a(href = "http://andybeger.com/author/andybeger/", "Andy Beger"),
      "added the check box for normalization."),
      plotOutput("lineplot")
    )
    
)))
