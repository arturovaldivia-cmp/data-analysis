#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("shiny")
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  h1("A/B test")
  ,hr()
  ,h2("Select test")
  ,fluidRow(
        column(3
              ,numericInput("testid", "Select test id:", 816)
              ,numericInput("Control_position", "Control's position in tours list:", 0)
              ,dateRangeInput("daterange", "Date range:",
                   start = "2016-06-13",
                   end   = "2016-12-21")
              ,radioButtons("ratioSelector"
                             ,"Select ratio to compare",inline=TRUE
                             ,c("EPC" = "R:P"
                                #,"U:F" = "U:F"
                                ,"F:P" = "F:P"
                               )
                            )
              ,radioButtons("autoupdate"
                            ,"Auto update?",inline=TRUE
                            ,c("No" = "no",
                               "Yes" = "yes"))
              ,actionButton("goButton", "Connect to data base")
              
              
        ),
        column(9
               ,h4("Summary")
               ,tableOutput('summaryTable')
               ,h4(textOutput("topSummary"))
               )
  )
  ,hr()
  
  ,h2("Inspection")
  ,fluidRow(
        column(3
               ,h4("Visits")
               # Paramaters
               ,numericInput("b1", "Number of visitors in Variation A:", 100)
               ,numericInput("b2", "Number of visitors in Variation B:", 100)
               )
        ,column(3
                ,h4("Sales")
               ,numericInput("a1", "Number of sales in Variation A:", 15)
               ,numericInput("a2", "Number of sales in Variation B:", 10)
               )
        ,column(3
               #,h4("Extras")
               ,sliderInput("scale",
                            "Simultate using a scale factor:",
                            min = 1,
                            max = 10,
                            value = 1)
               ,sliderInput("bins",
                           "# breaks:",
                           min = 1,
                           max = 100,
                           value = 1)
              )
        ,column(3
                ,h4(textOutput("pValue"))
                ,sliderInput("alpha"
                             ,"Significance level"
                             ,min = 1
                             ,max = 100
                             ,value = 95)
        )
          )
  ,textOutput("Result")
  ,hr()
  ,plotlyOutput("plot")
  ,hr()
  ,plotlyOutput("distPlot")
  ,fluidRow(
      column(5
             ,h4(textOutput("Summary"))
             ,tableOutput('summaryTable_bottom')
             )
            )
))
