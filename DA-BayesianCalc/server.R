#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CMP Data Analysis
# AV: Oct 2016
#
# Success ratio analysis (server)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library("shiny")
library("plotly")
library("dplyr")
library("lubridate")
library("reshape")

source("/Users/arturovaldivia/Google Drive/Free-style-tech/Bandit/Abby/DA-BayesianCalc copy/AV_BayesSA_library_copy.R")

textToNumber <- function(text){
                          number <- gsub(",","",text)
                          
                          number <- ifelse(TRUE                # ToDo: Check text length and decide if number 
                                                               #       should be converted to integer or to double
                                           ,as.integer(number)
                                           ,as.double(number)
                                           )
                          
                          return(number)
                  }

shinyServer(
            function(input, output) {
                
              output$distPlot <- renderPlotly({
                                              Attempts1 <<- textToNumber(input$b1)
                                              alpha1 <<- textToNumber(input$a1)
                                              Attempts2 <<- textToNumber(input$b1)
                                              alpha2 <<- textToNumber(input$a2)
                                              
                                              
                                             appSamples <<- app.Beta_samples(Shape1 = input$scale*c(alpha1, alpha2)
                                                                              ,Shape2 = input$scale*c(Attempts1, Attempts2)-input$scale*c(alpha1, alpha2)
                                                                              )
                                              
                                              Loss <<- app.Beta_MonteCarlo(Sample = appSamples)
                                              Delta <<- paste("Observed difference of "
                                                              ,round(100*(((alpha2/Attempts2)/(alpha1/Attempts1))-1),1)
                                                              ,"%", sep="")
                                              Plotly_distributions(Shape1 = input$scale*c(alpha1, alpha2)
                                                                   ,Shape2 = input$scale*(c(Attempts1, Attempts2)-c(alpha1, alpha2))
                                                                   ,Dispersion_length = 3
                                                                   ,Title_extra = paste("Probability of beating control ", round(100*(1-Loss),1), "%", sep="")
                                                                   ,plot_scope = 3, maxX_ = NULL
                                                                   ,Variant_names = c("Control", "Test")
                                                                  )
                                            })
                          
              output$plot <- renderPlotly({
                                          appSamples <<- app.Beta_samples(Shape1 = input$scale*c(alpha1, alpha2)
                                                                          ,Shape2 = input$scale*c(Attempts1, Attempts2)-input$scale*c(alpha1, alpha2)
                                                                          )
                                          H <<- hist(appSamples[,2]/appSamples[,1] -1, breaks = input$bins, plot = FALSE)
                                          
                                          Bad <<- as.data.frame(cbind(head(H$breaks,-1), H$counts))
                                          names(Bad) <- c("Difference", "Density")
                                          
                                          totalBad <- sum(Bad$Density)
                                          
                                          Bad$Contribution_positive <- round(100*(Bad$Density*(1-Loss)/totalBad),1)
                                          Bad$Contribution_negative <- round(100*(Bad$Density*Loss/totalBad),1)

                                          Bad$Density <- round(100*(Bad$Density/totalBad),1)
                                          Bad$Difference <- 100*Bad$Difference
                                          
                                          plot_ly(data = Bad %>% dplyr::filter(Difference<0)
                                                  , x = ~Difference
                                                  , y = ~Density
                                                  , type = 'bar'
                                                  ,marker = list(color = 'rgb(244, 66, 66)')
                                                  ,name = paste("Loss likelood ", round(100*Loss,1),"%",sep="")
                                                  )  %>%
                                            add_trace(data = Bad %>% dplyr::filter(Difference>=0)
                                                      , x = ~Difference
                                                      , y = ~Density
                                                      , type = 'bar'
                                                      ,name = paste("Win likelood ", round(100*(1-Loss),1),"%",sep="")
                                                      ,marker = list(color = 'rgb(81, 165, 86)')
                                                      )%>%
                                            layout(xaxis = list(range = 100*c(min(H$breaks),max(H$breaks))
                                                                ,title = "Difference (%)"
                                                                )
                                                  ,yaxis = list(title = "Probability (%)"
                                                                ,range = c(0,100)
                                                                )
                                                  ,title = "Control -vs- Test: Difference in Conversion"
                                                  )
                                          })
              
              output$pValue <- renderText({ 
                                          paste(
                                          ifelse(100*round(pnorm(abs(P_value(p1 = alpha1/(Attempts1)
                                                                             ,p2 = alpha2/(Attempts2)
                                                                             ,n1 = input$scale*Attempts1
                                                                             ,n2 = input$scale*Attempts2))),2) > input$alpha
                                                  ," Z-test Significant."
                                                  ," Z-test Not Significant."
                                                  )
                                           ,"(Z-score ="
                                             ,round(P_value(p1 = alpha1/(Attempts1)
                                                            ,p2 = alpha2/(Attempts2)
                                                            ,n1 = input$scale*Attempts1
                                                            ,n2 = input$scale*Attempts2
                                                            ,returnProb = FALSE),3)
                                             ,")"
                                          )
                                          })
              
              output$Result <- renderText({
                                            paste(paste("Observed difference of "
                                                        ,round(100*(((alpha2/Attempts2)/(alpha1/Attempts1))-1),1)
                                                        , "%", sep="")
                                                  , ". Probability of beating control ", round(100*(1-Loss),1), "%", sep="")
                                          })    
              
              
                        }
            )
