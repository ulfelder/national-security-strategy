shinyUI(fluidPage(
  titlePanel("Exploring the U.S. National Security Strategy Reports"),
  
  sidebarLayout(position = "right",
    
    sidebarPanel(textInput("term", h4("Input a Term"), value = "soviet")),
    
    mainPanel(
      p("This app lets you explore change over time in the national security concerns of the United States
        government by plotting term counts from all sixteen U.S. National Security Strategy Reports,
        published irregularly by the White House from 1987 to 2015."),
      p("Use the box on the right to input a term to plot. The plotting function will find all terms
        in the data set that include that term and will sum the counts of their appearances. For example,
        inputting 'afghan' will combine counts by report for the terms 'afghan', 'afghans', afghanistan',
        and 'afghanistans'. The function is not case sensitive."),
      p("The data and scripts used to build this app can be found on GitHub, ",
        a(href = "https://github.com/ulfelder/national-security-strategy", "here"),
      "."),
      br(),
      br(),
      plotOutput("lineplot")
    )
    
)))