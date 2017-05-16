library('ROCR')
library(shiny)
library(ggplot2)

calcon <- function(predictions,references,target){
  confusionMatrix <- table(truth = c(predictions==references), prediction = c(predictions==target))
  return (confusionMatrix)
}

shinyServer(function(input, output) {
  
  output$target <- renderUI({
    radioButtons("target", label = "choise male or female:", choices = list("male" = "male", "female" = "female"), 
                 selected = 1)
  })
  
  output$methods <- renderUI({
    methods_list = list.files("./methods/")
    checkboxGroupInput("methods", label = "choise methods:", choices = methods_list,selected = 1)
  })
  
  output$plot <- renderPlot({
    
    query_m = input$target
    files <- input$methods
    
    # read files
    senResult <- c() 
    speResult <- c()
    
    for(file in files)
    {
      d<-read.table(paste0("./methods/", file), header=T,sep=",")
      cal<-calcon(d$prediction,d$reference,query_m)
      calsen <- round((cal[4]/(cal[4]+cal[1])), digits = 2)
      calspe <- round((cal[2]/(cal[2]+cal[3])), digits = 2)
      
      senResult <- c(senResult, calsen) 
      speResult <- c(speResult, calspe)
    }
    
    # get the plot graph
    data = do.call(rbind.data.frame, Map('c', senResult, speResult))
    ggplot(data, aes(x=speResult, y=senResult)) + geom_point()
    
  })
  output$table <- renderTable({
    
    query_m = input$target
    files <- input$methods
    
    senResult <- c() 
    speResult <- c()
    methods <- c()
    
    for(file in files) {
      d <- read.table(paste0("./methods/",file), header = T, sep = ",")
      
      method <- gsub(".csv", "", basename(file))
      d<-read.table(paste0("./methods/", file), header=T,sep=",")
      cal<-calcon(d$prediction,d$reference,query_m)
      calsen <- round((cal[4]/(cal[4]+cal[1])), digits = 2)
      calspe <- round((cal[2]/(cal[2]+cal[3])), digits = 2)
      
      senResult <- c(senResult, calsen) 
      speResult <- c(speResult, calspe)
      methods <- c(methods, method)

    }
    
    data <- cbind(method=methods, calsen=senResult, calspe=speResult)
    data
    
    
  })     
})
