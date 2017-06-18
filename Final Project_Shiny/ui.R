library(shiny)
library(ggplot2)

shinyUI(navbarPage("HR Analytics",
                   tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                            h3(" About the analysis:", style = "color:blue"),
                            h3(""),
                            h5("Hi, this analysis is base on a fictional data \"HR Employee Attrition and Performance\" which is created by IBM data scientists."),
                            h5(" You can download the file in original web path:https://www.ibm.com/communities/analytics/watson-analytics-blog/hr-employee-attrition/"),
                            h3(" About author", style = "color:blue"),
                            h5("Name: Jimmy Hsu"),
                            h5("Email: winterthink@gmail.com"),
                            h5("Specialties Areas: "),
                            h5("＊EnCase Certificated Instructor"),
                            h5("＊Digital Forensic Analysis"),
                            h5("＊Information Security"),width = "400px"),
                        mainPanel(
                            h3(" About the data:", style = "color:blue"),
                            tableOutput("table_of_rawdata")
                            ))),
                   navbarMenu("Salary Issue",
                            tabPanel("Age and Salary",
                                      plotOutput("plot_GenderAgeMoney")),
                            tabPanel("Total Working Years and Salary",
                                     plotOutput("plot_Totalworking")),
                            tabPanel("Years In Role and Salary",
                                     plotOutput("plot_YearsInRole")),
                            tabPanel("Years At Company and Salary",
                                     plotOutput("plot_YearsAtCompany")),
                            tabPanel("Merge all Picture",
                                     imageOutput("pic_SalaryMergeAll"))
                   ),

                  navbarMenu("Work life balance Issue",
                            tabPanel("Balance between Work and life is Best",
                                     plotOutput("plot_WorklifebalanceBest")),
                            tabPanel("Balance between Work and life is Better",
                                     plotOutput("plot_WorklifebalanceBetter")),
                            tabPanel("Balance between Work and life is Good",
                                     plotOutput("plot_WorklifebalanceGood")),
                            tabPanel("Balance between Work and life is Bad",
                                    plotOutput("plot_WorklifebalanceBad")),
                            tabPanel("Merge all Picture",
                                     imageOutput("pic_WorklifebalanceMergeAll"))
                  ),                                     
                  
                  navbarMenu("Job Satisfaction Issue",
                             tabPanel("Job Satisfaction is Best",
                                      plotOutput("plot_JobSatisfactionBest")),
                             tabPanel("Job Satisfaction is Better",
                                      plotOutput("plot_JobSatisfactionBetter")),
                             tabPanel("Job Satisfaction is Good",
                                      plotOutput("plot_JobSatisfactionGood")),
                             tabPanel("Job Satisfaction is Bad",
                                      plotOutput("plot_JobSatisfactionBad")),
                             tabPanel("Merge all Picture",
                                      imageOutput("pic_JobSatisfactionMergeAll"))
                  )                  
                  
                  
))


