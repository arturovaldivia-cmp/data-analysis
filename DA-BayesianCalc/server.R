#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("/Users/arturovaldivia/Google Drive/Free-style-tech/Bandit/AV_BayesSA_library.R")
source("/Users/arturovaldivia/Google Drive/LTV/LTV_phase2/AV_library.R")

library("dplyr")
library("plotly")
library("lubridate")
library("reshape")
library("RMySQL")

Close_db_connections()
Adminix <- DB_Connect()

# Define server logic required to draw a histogram
shinyServer(
            function(input, output) {
              lastSummary <<- c()
              impact <<- NA
              Impact <<- "Impact: "
              
              # Fetch and store data from a tour
              output$summaryTable_bottom <- renderTable(align = 'c', digits = 0, expr = {
                                          if(input$goButton & is.numeric(input$testid) & input$autoupdate == "yes"){
                                                          print(paste("Processing...", input$testid))
                                                          if(TRUE){
                                                                                # Tours and dates to check ----------------------------------------------
                                                                                
                                                                                Tours =  suppressWarnings(fetch(dbSendQuery(Adminix
                                                                                                                            ,paste("SELECT tour_id FROM go.split_tours WHERE split_log_id =",input$testid)
                                                                                                                            )
                                                                                                                )
                                                                                                          )$tour_id

                                                                                numTours = length(Tours)
                                                                                
                                                                                Control_variation = Tours[1]
                                                                                
                                                                                # Get metrics ----------------------------------------------
                                                                                
                                                                                if(TRUE){
                                                                                  paids_ <- suppressWarnings(fetch(dbSendQuery(Adminix,get.paids.sql(Tours = Tours)),n=-1))
                                                                                  frees_ <- suppressWarnings(fetch(dbSendQuery(Adminix,get.frees.sql(Tours = Tours)),n=-1))
                                                                                  Frees_and_Paids_ <<- dplyr::left_join(frees_, paids_) %>% 
                                                                                                      dplyr::select(-isTestUser, -email)
                                                                                
                                                                                  Raws_and_Uniques_ <<- suppressWarnings(fetch(dbSendQuery(Adminix,get.raws_and_uniques.sql(Tours = Tours)),n=-1))
                                                                                  
                                                                                }# Update metrics
                                                                                
                                                                                # Group by time unit ----------------------------------------------
                                                                                
                                                                                if(TRUE){
                                                                                  N = length(Tours)
                                                                                  
                                                                                  Frees_and_Paids <<- dplyr::left_join(frees_  %>%
                                                                                                                       mutate(thedate = as.POSIXct(signup_time, 
                                                                                                                                                     format="%Y-%m-%d", origin = "1970-01-01 00:00:00",tz = "EST")) %>%
                                                                                                                       group_by(thedate, tour_id) %>%
                                                                                                                       summarise(Frees = n_distinct(smoochy_user_id))
                                                                                                                      ,paids_  %>%
                                                                                                                       mutate(thedate = as.POSIXct(join_date, 
                                                                                                                                                    format="%Y-%m-%d", origin = "1970-01-01 00:00:00",tz = "EST"))%>%
                                                                                                                       group_by(thedate, tour_id) %>%
                                                                                                                       summarise(Sales = n_distinct(cmp_member_id)
                                                                                                                                  ,Hidden = sum(Hidden, na.rm=TRUE)
                                                                                                                                  ,Paid = Sales - Hidden
                                                                                                                                  )
                                                                                                                      )
                                                                                  
                                                                                  Raws_and_Uniques <<- Raws_and_Uniques_ %>%
                                                                                                       mutate(thedate = as.POSIXct(thedate, 
                                                                                                                                    format="%Y-%m-%d", origin = "1970-01-01 00:00:00",tz = "EST")) %>%
                                                                                                       group_by(thedate,tour_id) %>%
                                                                                                       summarise(Raws = sum(raws, na.rm=TRUE)
                                                                                                                ,Uniques = sum(uniques, na.rm=TRUE)
                                                                                                                ) 
                                                                                  
                                                                                  activityRange <- c(min(Raws_and_Uniques$thedate), max(Raws_and_Uniques$thedate))
                                                                                  
                                                                                  `Test data` <<- dplyr::left_join(Raws_and_Uniques
                                                                                                                   ,Frees_and_Paids %>% dplyr::filter(thedate <= activityRange[2])
                                                                                                                   )
                                                                                  if(FALSE){
                                                                                  Days <<- unique(Frees_and_Paids$thedate)
                                                                                  N <<- length(Tours)
                                                                                  
                                                                                  `Test data` <<- dplyr::left_join(as.data.frame(cbind(rep(Tours, times =length(Days)), rep(Days, each = N))
                                                                                                                                 ,stringsAsFactors = FALSE) %>%
                                                                                                                   mutate(V1 = as.integer(V1))
                                                                                                                 ,dplyr::left_join(Raws_and_Uniques,Frees_and_Paids)
                                                                                                                 ,by = c("V1" = "tour_id", "V2" = "thedate")
                                                                                                                  ) %>%
                                                                                                  mutate(Day = as.POSIXct(V2, format="%Y-%m-%d", origin = "1970-01-01 00:00:00", tz="EST"))
                                                                                              
                                                                                  names(`Test data`)[1:2] <<- c("tour_id","thedate")
                                                                                  Days <<- unique(`Test data`$Day)
                                                                                  numDays <<- length(Days)
                                                                                  }
                                                                                  
                                                                                }# By day
                                                                                
                                                                                lastSummary <<- Summary <- `Test data` %>%
                                                                                                             mutate(Raws = printNumbers(sum(Raws, na.rm=TRUE))
                                                                                                                      ,Uniques = printNumbers(sum(Uniques, na.rm=TRUE))
                                                                                                                      ,Frees = printNumbers(sum(Frees, na.rm=TRUE))
                                                                                                                      ,Sales = sum(Sales, na.rm=TRUE)
                                                                                                                      ,Hidden = sum(Hidden, na.rm=TRUE)
                                                                                                                      ,Paid = Sales - Hidden
                                                                                                                      ,Sales = printNumbers(Sales)
                                                                                                                      ,Hidden = printNumbers(Hidden)
                                                                                                                      ,Paid = printNumbers(Paid)
                                                                                                                      ) 
                                            saveRDS(object = `Test data`
                                                    ,file = paste("~/Google Drive/temp/Test_",input$testid,".rds",sep=""))
                                                          }

                                                          Summary
                                                          }else{lastSummary}
                                          })
              
              output$summaryTable <- renderTable({if(file.exists(paste("~/Google Drive/temp/Test_",input$testid,".rds",sep=""))){
                                                    aux <- readRDS(paste("~/Google Drive/temp/Test_",input$testid,".rds",sep=""))
                                                  
                                                    availableDates <<- c(head(aux$thedate,1) , tail(aux$thedate,1)) 
                                                    
                                                    Aux <- aux %>%
                                                            mutate(Day = as.Date(thedate, "%m/%d/%Y", tz = "EST")) %>%
                                                            dplyr::filter(Day >= input$daterange[1]) %>%
                                                            dplyr::filter(Day <= input$daterange[2]) %>%
                                                            group_by(tour_id) %>%
                                                            summarise(Raws = sum(Raws, na.rm=TRUE)
                                                                      ,Uniques = sum(Uniques, na.rm=TRUE)
                                                                      #,Uniques = printNumbers(sum(Uniques, na.rm=TRUE))
                                                                      ,Frees = sum(Frees, na.rm=TRUE)
                                                                      #,Frees = printNumbers(sum(Frees, na.rm=TRUE))
                                                                      ,Sales = sum(Sales, na.rm=TRUE)
                                                                      ,Hidden = sum(Hidden, na.rm=TRUE)
                                                                      ,Paid = Sales - Hidden
                                                                      )

                                                    numTours <- length(Aux$tour_id)
                                                    controlPosition <- 1+input$Control_position%%numTours
                                                    controlTour <- Aux$tour_id[controlPosition]
                                                    variantNames <- Aux$tour_id
                                                    
                                                    # Get variables to be compared  
                                                    attempts_ <- switch(input$ratioSelector
                                                                        ,`R:P` = c("Raws","Paid")
                                                                        ,`U:F` = c("Uniques","Frees")
                                                                        ,`F:P` = c("Frees","Paid")
                                                                        )
                                                    successes_ <- attempts_[2]
                                                    attempts_ <- attempts_[1]
                                                    
                                                    controlAttempts <- Aux[controlPosition, attempts_]
                                                    controlSuccesses <- Aux[controlPosition, successes_]
                                                    ratioControl <- controlSuccesses/controlAttempts
                                                    
                                                    # Compute z-score
                                                    if(TRUE){
                                                    Aux$z <- NA
                                                    for(variant_ in variantNames){
                                                      variantPosition <- match(variant_, Aux$tour_id)
                                                      
                                                      Aux$z[variantPosition] <- P_value(p1 = ratioControl
                                                                                        ,n1 = Aux[controlPosition, attempts_]
                                                                                        ,p2 = Aux[variantPosition, successes_]/Aux[variantPosition, attempts_]
                                                                                        ,n2 = Aux[variantPosition, attempts_]
                                                                                        )[[1]]
                                                    }
                                                    
                                                    Aux$z <- ifelse(pnorm(abs(Aux$z)) > 95#input$alpha/100
                                                                    ,"Significant"
                                                                    ,"Not Significant"
                                                                    )
                                                    }
                                                    
                                                    # Compute all posible comparisons
                                                    # re-scale in case Shape2 exceeds the 1M threshold
                                                    rescaleFactor <- ifelse(sum(controlAttempts>1e9)>0 & attempts_ == "Raws",1e-3,1) 
                                                    
                                                    appSamples1 <- app.Beta_samples(Shape1 = array(unlist(Aux[,successes_]))
                                                                                    ,Shape2 = round(array(unlist(Aux[, attempts_]))*rescaleFactor)-array(unlist(Aux[,successes_]))
                                                                                    ,variantsNames = Aux$tour_id
                                                                                    )
                                                    Losses_ <- app.Beta_MonteCarlo(Sample = appSamples1)
                                                    
                                                    # Keep only pairs involving of the type Variant > Control Tour
                                                    rightPattern <- paste(">", as.character(controlTour),sep="")
                                                    Losses_BA <- Losses_[grepl(rightPattern, rownames(Losses_))]
                                                    namesLosses_BA <- gsub(rightPattern,"",rownames(Losses_)[grepl(rightPattern, rownames(Losses_))])
                                                    
                                                    # Reverse comparisons of the type Control Tour > Variant 
                                                    reversedPattern <- paste(as.character(controlTour),">",sep="")
                                                    Losses_AB <- 1-Losses_[grepl(reversedPattern,rownames(Losses_))]
                                                    namesLosses_AB <- gsub(reversedPattern,"",rownames(Losses_)[grepl(reversedPattern, rownames(Losses_))])

                                                    lossesNames <- as.integer(c(namesLosses_BA,as.character(controlTour),namesLosses_AB))
                                                    
                                                    # Put everything together
                                                    Losses_ <- round(100*(c(Losses_BA,0,Losses_AB)),1)
                                                    Losses_ <- as.data.frame(t(rbind(lossesNames,Losses_)),stringsAsFactors = FALSE)
                                                    names(Losses_) <- c("tour_id","Confidence")
                                                    rownames(Losses_) <- 1:length(rownames(Losses_))
                                                    if(FALSE){
                                                    Labels_ <- matrix(ncol = 3, nrow = numTours)
                                                    Labels_[,3] <- paste("A>", LETTERS[1:numTours], sep = "")
                                                    Labels_[,2] <- LETTERS[1:numTours]
                                                    Labels_[1,1] <- controlTour
                                                    Labels_[2:numTours,1] <- sort(unique(setdiff(Aux$tour_id, c(Labels_[1,1]))))
                                                    Labels_ <- as.data.frame(Labels_,stringsAsFactors = FALSE)
                                                    names(Labels_) <- c("tour_id", "Variation", "Comparison")
                                                    
                                                    Losses_ <- round(100*t(c(0,1-app.Beta_MonteCarlo(Sample = appSamples1)[1:(length(Aux$tour_id)-1)])),2)
                                                    colnames(Losses_)[1] <- "A>A"
                                                    Losses_ <- as.data.frame(cbind(t(Losses_),colnames(Losses_))
                                                                            ,stringsAsFactors = FALSE)
                                                    names(Losses_) <- c("Confidencce", "Comparison")
                                                    }
                                                    
                                                    
                                                    # Impact analysis
                                                    totalRaws <- sum(Aux$Raws, na.rm = TRUE)
                                                    totalPaid <- sum(Aux$Paid, na.rm = TRUE)
                                                    totalEPC <- (totalPaid*50)/(totalRaws/1000)
                                                    controlEPC <- (controlSuccesses*50)/(controlAttempts/1000)
                                                    
                                                    actualRevenue <- totalEPC*(totalRaws/1000)
                                                    potentialRevenue <- controlEPC*(totalRaws/1000)
                                                    
                                                    Impact <<- actualRevenue-potentialRevenue
                                                    
                                                    output$topSummary <<- renderText({ifelse(input$ratioSelector == "R:P"
                                                                                         ,paste("Impact of this test:", printMoney(Impact))
                                                                                         ,"")
                                                                                         })
                                                    
                                                    
                                                    # Format output
                                                    Aux <- dplyr::inner_join(Aux ,Losses_) %>%
                                                           mutate(Hidden = printNumbers(Hidden)
                                                                  ,`R:P` = paste("1:",round(Raws/Paid), sep = "")
                                                                  ,`F:P` = paste("1:",round(Raws/Frees), sep = "")
                                                                  ,Paid = printNumbers(Paid)
                                                                  ,Raws = format(round(Raws, 0), nsmall=0)
                                                                  ,Uniques = format(round(Uniques, 0), nsmall=0)
                                                                  ,tour_id = format(round(tour_id, 0), nsmall=0)
                                                                  ,BayesianConfidence = ifelse(max(Confidence, 100-Confidence) > input$alpha,"Significant", "Not Significant")
                                                                  ,Confidence = paste(as.double(Confidence),"%",sep = "")
                                                                  ) %>%
                                                           dplyr::select(-Sales, -Hidden)
                                                         
                                                    Aux$z[controlPosition] <- "Control"
                                                    Aux$Confidence[controlPosition] <- "Control"
                                                    Aux$BayesianConfidence[controlPosition] <- "Control"
                                                    
                                                    names(Aux)[names(Aux) %in% c("tour_id","z","Confidence","BayesianConfidence")] <- c("Tour"
                                                                                                                                        ,"Significant (z-test)"
                                                                                                                                        ,"Beat Control prob. (Bayesian)"
                                                                                                                                        ,"Significant (Bayesian)")
                                                    
                                                    
                                                    Aux[,c("Tour", "Raws", "Uniques", "Frees", "Paid", "R:P", "Significant (z-test)"
                                                           ,"R:P","F:P", "Significant (Bayesian)","Beat Control prob. (Bayesian)")]
                                                  }
                                    })
              
              output$Summary <- renderText({
                                            ifelse(input$goButton,"Summary","")
                                            })
                          
              output$distPlot <- renderPlotly({
                                             appSamples <<- app.Beta_samples(Shape1 = input$scale*c(input$a1, input$a2)
                                                                              ,Shape2 = input$scale*c(input$b1, input$b2)-input$scale*c(input$a1, input$a2)
                                                                              )
                                              
                                              Loss <<- app.Beta_MonteCarlo(Sample = appSamples)
                                              Delta <<- paste("Observed difference of "
                                                              ,round(100*(((input$a2/input$b2)/(input$a1/input$b1))-1),1)
                                                              ,"%", sep="")
                                              Plotly_distributions(Shape1 = input$scale*c(input$a1, input$a2)
                                                                   ,Shape2 = input$scale*(c(input$b1, input$b2)-c(input$a1, input$a2))
                                                                   ,Dispersion_length = 3
                                                                   ,Title_extra = paste("Probability of beating control ", round(100*(1-Loss),1), "%", sep="")
                                                                   ,plot_scope = 3, maxX_ = NULL
                                                                   ,Variant_names = c("Control", "Test")
                                                                  )
                                            })
                          
              output$plot <- renderPlotly({
                                          appSamples <<- app.Beta_samples(Shape1 = input$scale*c(input$a1, input$a2)
                                                                          ,Shape2 = input$scale*c(input$b1, input$b2)-input$scale*c(input$a1, input$a2)
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
                                          ifelse(100*round(pnorm(abs(P_value(p1 = input$a1/(input$b1), p2 = input$a2/(input$b2)
                                                               ,n1 = input$scale*input$b1, n2 = input$scale*input$b2))),2) > input$alpha
                                                  ," Z-test Significant."
                                                  ," Z-test Not Significant."
                                                  )
                                           ,"(Z-score ="
                                             ,round(P_value(p1 = input$a1/(input$b1), p2 = input$a2/(input$b2)
                                                            ,n1 = input$scale*input$b1, n2 = input$scale*input$b2, returnProb = FALSE),3)
                                             ,")"
                                          )
                                          })
              
              output$Result <- renderText({
                                            paste(paste("Observed difference of "
                                                        ,round(100*(((input$a2/input$b2)/(input$a1/input$b1))-1),1)
                                                        , "%", sep="")
                                                  , ". Probability of beating control ", round(100*(1-Loss),1), "%", sep="")
                                          })    
              
              
                        }
            )
