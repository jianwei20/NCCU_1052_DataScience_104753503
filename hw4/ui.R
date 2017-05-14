library(shiny)
shinyUI(fluidPage(
  titlePanel("NCCU_Data Science_HW4_105753503"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label = h3("Radio buttons"), choices = list("male" = "male", "female" = "female"), 
                   selected = 1),
      checkboxGroupInput("checkGroup", label = h3("Method"), choices = list("Method 1" = "method1", "Method 2" = "method2", "Method 3" = "method3", "Method 4" = "method4", "Method 5" = "method5", "Method 6" = "method6", "Method 7" = "method7", "Method 8" = "method8", "Method 9" = "method9", "Method 10" = "method10"),
                         selected = 1)
    ),
    mainPanel(plotOutput("distPlot"))
  )
))