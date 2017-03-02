#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# CMP Data Analysis
# AV: Oct 2016
#
# Sequential Analysis library
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Required libraries ------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library("dplyr")
library("plotly")
library("lubridate")
library("reshape")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compute first two moments ------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beta_mean <- function(shape1 = 15, shape2 = 100){
  return(shape1/(shape1+shape2))
}

beta_var <- function(shape1 = 15, shape2 = 100){
  return((shape1*shape2)/(((shape1+shape2)^2)*(shape1+shape2+1)))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compute p-value evolution ----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
P_value <- function(p1, p2, n1, n2, model = 1, returnProb = TRUE){
  
  if(model != 1){
    p = (p1*n1+p2*n2)/(n1+n2)
    
    SE = sqrt(p*(1-p)*(1/n1 + 1/n2))
    
    #SE = sqrt((p1*(1-p1) + p2*(1-p2))/(n1+n2))
    
  }else{
    
    SE = sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2)
  }
  
  
  z = (p2-p1)/SE
  
  return(z)#ifelse(returnProb,pnorm(abs(z)),z))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Success ratio analysis app
# Compute Monte Carlo estimate ----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app.Beta_samples <- function(Sample_size = 50e4
                            ,Shape1 = c(50, 14, 140)
                            ,Shape2 = c(70, 80, 140)
                            ,variantsNames = NULL
                            ){
                              
                              numVariants = length(Shape1)
                              
                              Samples <- matrix(nrow = Sample_size, ncol = numVariants)  
                              

                              
                              for(variation_ in 1:numVariants){
                                Samples[,variation_] <- rbeta(n = Sample_size,shape1 = Shape1[variation_],shape2 = Shape2[variation_])
                              }
                              
                              if(is.null(variantsNames)){
                                        colnames(Samples) <- LETTERS[1:numVariants]
                              }else{
                                        colnames(Samples) <- as.character(variantsNames)
                              }
                              
                              return(Samples)
                            }

app.Beta_MonteCarlo <- function(Sample = app.Beta_samples()){
  
  Sample_size = dim(Sample)[1] # Number of rows
  numVariants = dim(Sample)[2] # Number of colums
  
  Comparisons <- combn(colnames(Sample),2)
  Results <- colnames_ <- array(dim = length(Comparisons[1,]))
  
  for(comb_ in 1:length(colnames_)){
    colnames_[comb_] <- paste(Comparisons[,comb_],collapse = ">")
    
    Results[comb_] <- sum(Sample[,Comparisons[1,comb_]] > Sample[,Comparisons[2,comb_]])/Sample_size
  }
  
  rownames(Results) <- colnames_
  
  return(Results)
}


app.Beta_MonteCarlo_uplift <- function(Sample = app.Beta_samples()
                                       ,uplift = .10
                                       ){
  
  Sample_size = dim(Sample)[1] # Number of rows
  numVariants = dim(Sample)[2] # Number of colums
  
  Comparisons <- combn(colnames(Sample),2)
  Results <- colnames_ <- array(dim = length(Comparisons[1,]))
  
  for(comb_ in 1:length(colnames_)){
    colnames_[comb_] <- paste(Comparisons[,comb_],collapse = ">")
    
    Results[comb_] <- sum(Sample[,Comparisons[1,comb_]] > (1+uplift)*Sample[,Comparisons[2,comb_]])/Sample_size
  }
  
  rownames(Results) <- colnames_
  
  return(Results)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Graphic tools -----------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Plot_distributions <- function(Shape1 = c(28, 15, 20, 14)
                               ,Shape2 = c(4473, 1398, 1000, 1090)
                               ,Dispersion_length = 3
                               ,Variant_names = NULL#c("A", "B", "C", "D")
                               ,Colors = c("blue", "red", "green", "black")
                               ,maxX_ = NULL
                               ,Title_extra = ""
                               ,Legend_side = "topleft"
                               ,plot_scope = 3
){
  if(is.null(Variant_names)){
    Variant_names = LETTERS[1:length(Shape1)]
  }
  
  
  X <- seq(0, 1, length=5000)
  
  distribution <- matrix(nrow = length(X), ncol = length(Shape1))
  moments = matrix(nrow = length(Shape1), ncol = 2)
  rownames(moments) = Variant_names
  colnames(moments) = c("mean", "stddev")
  
  for(j in 1:length(Shape1)){
    
    distribution[,j] = dbeta(x = X, shape1 = Shape1[j], shape2 = Shape2[j])
    
    for(m_ in 1:2){
      moments[j,1] = beta_mean(shape1 = Shape1[j], shape2 = Shape2[j])
      moments[j,2] = sqrt(beta_var(shape1 = Shape1[j], shape2 = Shape2[j]))
    }
  } # Distribution details 
  
  maxY = max(distribution, na.rm = TRUE)
  
  if(is.null(maxX_)==TRUE){
    maxX <- array(dim = 2)
    maxX[1] <- min(moments[,1]-plot_scope*moments[,2])
    maxX[2] <- max(moments[,1]+plot_scope*moments[,2])
    
    if(sum(is.finite(maxX))>=1){maxX_ <- c(0,1)}
  }
  
  for(j in 1:length(Shape1)){
    plot(X
         ,distribution[,j]
         ,type="l", lty=1, xlab="",ylab="", ylim = c(0,maxY), xlim = maxX
         ,yaxt = 'n', xaxt = 'n'
         ,lwd = 2, col = Colors[j]
    )
    abline(v = moments[j,1]+Dispersion_length*c(-1,1)*moments[j,2],lty = 3, col = Colors[j])
    abline(v = moments[j,1], lwd = 2, lty = 2, col = Colors[j])
    par(new = TRUE)
  }
  axis(side=2, at=seq(0,maxY,maxY/4), labels=paste(seq(0,100,25),"%",sep=""))
  axis(side=1, at=seq(maxX[1],maxX[2],maxX[2]/10), labels=paste(round(100*seq(maxX[1],maxX[2],maxX[2]/10),2),"%",sep=""))
  
  title(main = Title_extra#paste("Distribution of Conversion Rates", Title_extra, sep = "")
        ,xlab = "Conversion parameter"
        ,ylab = "Likelihood"
  )
  legend(Legend_side, inset=.05, title="Distributions parameters"
         ,legend = c(Variant_names, rep("Expected", length(Variant_names)))
         , lwd=2, lty=rep(c(1,2),each=length(Shape1)), col=rep(Colors,length(Shape1))
         ,bty='n'
  )
}

Plotly_distributions <- function(Shape1 = c(842, 809)
                                ,Shape2 = c(42166-842, 41134-809)
                                ,Dispersion_length = 3
                                ,Colors = c("rgb(244, 66, 66)", "rgb(81, 165, 86)", "green", "black")
                                ,maxX_ = NULL
                                ,Title_extra = ""
                                ,Legend_side = "topleft"
                                ,Variant_names = NULL
                                ,plot_scope = 3
                                ,Xaxis_title = "Conversion parameter"
){
  
  numVariants <- length(Shape1)
  
  if(is.null(Variant_names)){
  Variant_names <- LETTERS[1:numVariants]
  }
  
  X <- seq(0, 1, length=5000)
  
  distribution <- matrix(nrow = length(X), ncol = numVariants)
  moments = matrix(nrow = numVariants, ncol = 2)
  rownames(moments) = Variant_names
  colnames(moments) = c("mean", "stddev")
  
  # re-scale in case Shape2 exceeds the 1M threshold
  rescaleFactor <- ifelse(sum(Shape2>1e6)>0,1e-3,1)
  
  for(j in 1:length(Shape1)){
    
    distribution[,j] = dbeta(x = X, shape1 = Shape1[j], shape2 = Shape2[j]*rescaleFactor)
    
    for(m_ in 1:2){
      moments[j,1] = beta_mean(shape1 = Shape1[j], shape2 = Shape2[j]*rescaleFactor)
      moments[j,2] = sqrt(beta_var(shape1 = Shape1[j], shape2 = Shape2[j]*rescaleFactor))
    }
  } # Distribution details 
  
  maxY = max(distribution, na.rm = TRUE)
  
  if(is.null(maxX_)==TRUE){
    maxX <- array(dim = 2)
    maxX[1] <- min(moments[,1]-plot_scope*moments[,2])
    maxX[2] <- max(moments[,1]+plot_scope*moments[,2])
    
    if(sum(is.finite(maxX))==0){maxX_ <- c(0,1)}
  }
  
  P <- plot_ly(x = 100*X
               ,y = round(100*(distribution[,1]/maxY))
               ,name = Variant_names[1]
               ,type = 'scatter', mode = 'lines'
               ,line = list(color = Colors[1])
               ) %>%
       layout(xaxis = list(range = 100*maxX, title = Xaxis_title))
  
  for(j in 2:length(Shape1)){
    P <- add_trace(P
                   ,x =  100*X
                   ,y = round(100*(distribution[,j]/maxY))
                   ,name = Variant_names[j]
                   ,line = list(color = Colors[j])
                   ,type = 'scatter', mode = 'lines'
                   )
  }
  
  if(TRUE){
  # initiate a line shape object
  line <- list(
                type = "line",
                line = list(color = "pink", dash = "dash"),
                xref = "x",
                yref = "y"
              )
  lines <- list()
  
    for (j in 1:2) {
      line[["x0"]] <- 100*moments[j,1]
      line[["x1"]] <- 100*moments[j,1]
      line[["y0"]] <- 0
      line[["y1"]] <- 100
      line[["line"]]$color <- Colors[j]
      lines <- c(lines, list(line))
    }
  } # Prepare vertical lines
  
  P <- layout(P
             ,title = Title_extra
             ,shapes = lines
             ,yaxis = list(range = c(0,100), title= "Likelihood")
        ) %>%
        add_annotations(x = t(100*moments[,1])
                        ,y = c(50,50)
                        ,text = paste(as.character(t(round(100*moments[,1], 4))),"%",sep = "")
                        ,arrowhead = 4
                        ,arrosize = .5
                        ,ax = c(20,-20)
                        ,ay = c(-40,40)
                       )
  return(P)
}
