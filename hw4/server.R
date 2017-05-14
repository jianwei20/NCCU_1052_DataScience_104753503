library('ROCR')
library(shiny)
library(ggplot2)

calcon <- function(predictions,references,target){
  confusionMatrix <- table(truth = c(predictions==references), prediction = c(predictions==target))
  return (confusionMatrix)
}

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    query_m <- input$rbTarget
    files <- input$cbgMethod
    
    if(is.null(files)) {
      return()
    }
    
    # read files
    senResult <- c() 
    speResult <- c()
    
    for(file in files)
    {
      d<-read.table(file, header=T,sep=",")
      cal<-calcon(d$prediction,d$reference,query_m)
      calsen <- round((cal[4]/(cal[4]+cal[1])), digits = 2)
      calspe <- round((cal[2]/(cal[2]+cal[3])), digits = 2)
      
      senResult <- c(senResult, calsen) 
      speResult <- c(speResult, calspe)
    }
    
    x <- senResult
    y <- speResult
    data <- data.frame(senResult,speResult)
    qplot(x,y,data = data)
    
  })
})

