#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CMP Data Analysis
# AV: Oct 2016
#
# Success ratio analysis (UI)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library("shiny")
library(plotly)

shinyUI(fluidPage(
  
  h1("Success ratio analysis")
  ,hr()
  
  ,h2("Inspection")
  ,fluidRow(
        column(3
               ,h4("Attempts")
               # Paramaters
               ,numericInput("b1", "Number of attempts in Control:", 100)
               ,numericInput("b2", "Number of attempts in Test:", 100)
               )
        ,column(3
                ,h4("Successes")
               ,numericInput("a1", "Number of successes in Control:", 15)
               ,numericInput("a2", "Number of successes in Test:", 10)
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
