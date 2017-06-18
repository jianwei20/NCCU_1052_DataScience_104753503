library(RCurl)
library(shiny)
library(ggplot2)
library(bitops)
library(easyGgplot2)
library(mgcv)

# read files
URL <- "https://raw.githubusercontent.com/winterthink/NCCU_1052_DataScience_104753503/master/WA_Fn-UseC_-HR-Employee-Attrition.csv"
finalfile <- getURL(URL)
rawdata <- read.csv(textConnection(finalfile))

## create subset of salary components 
df_GenderAgeMoney <- data.frame(c(rawdata$Age), c(rawdata$MonthlyIncome), c(rawdata$Gender))
df_WorkingyersMonthlyincome <- data.frame(Totalworking =c(rawdata$TotalWorkingYears), YearsAtCompany= c(rawdata$YearsAtCompany), YearsInRole= c(rawdata$YearsInCurrentRole), MonthlyIncome= c(rawdata$MonthlyIncome))
## create subset of Worklifebalance components 
df_aboutDistance <- data.frame(c(rawdata$Age), c(rawdata$WorkLifeBalance), c(rawdata$JobSatisfaction), c(rawdata$DistanceFromHome))
SubWorklifebalance_Best <- subset(df_aboutDistance,(df_aboutDistance$c.rawdata.WorkLifeBalance.==4))
SubWorklifebalance_Better <- subset(df_aboutDistance,(df_aboutDistance$c.rawdata.WorkLifeBalance.==3))
SubWorklifebalance_Good <- subset(df_aboutDistance,(df_aboutDistance$c.rawdata.WorkLifeBalance.==2))
SubWorklifebalance_Bad <- subset(df_aboutDistance,(df_aboutDistance$c.rawdata.WorkLifeBalance.==1))
## create subset of Job Satisfaction
df_JobSatisfaction <- data.frame(c(rawdata$MonthlyIncome), c(rawdata$DistanceFromHome), c(rawdata$JobSatisfaction))
SubJobSatisfaction_Best <- subset(df_JobSatisfaction,(df_JobSatisfaction$c.rawdata.JobSatisfaction.==4))
SubJobSatisfaction_Better <- subset(df_JobSatisfaction,(df_JobSatisfaction$c.rawdata.JobSatisfaction.==3))
SubJobSatisfaction_Good <- subset(df_JobSatisfaction,(df_JobSatisfaction$c.rawdata.JobSatisfaction.==2))
SubJobSatisfaction_Bad <- subset(df_JobSatisfaction,(df_JobSatisfaction$c.rawdata.JobSatisfaction.==1))


shinyServer(function(input, output) {

  output$table_of_rawdata <- renderTable({
    table_of_rawdata <- table(data.frame(rawdata$Age,rawdata$Gender))
  })  
  
##################
## Salary issue ##   
##################

## Gender, Age and Salary ##  
  output$plot_GenderAgeMoney <- renderPlot({
  ggplot(df_GenderAgeMoney, aes(x=df_GenderAgeMoney$c.rawdata.Age., y=df_GenderAgeMoney$c.rawdata.MonthlyIncome.)) + geom_point(size = 1, color=df_GenderAgeMoney$c.rawdata.Gender.+1)+ geom_smooth()+ggtitle(expression(atop("Salary Analysis_Age and Gender",atop("")))) 

})

## Total Working Years and Salary ##  
  output$plot_Totalworking <- renderPlot({
    ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$Totalworking))+ geom_point(size = 1) +geom_smooth() +geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")+ggtitle(expression(atop("Salary Analysis_Total Working Years",atop("")))) 

})

## Years In Role and Salary ##  
   output$plot_YearsInRole <- renderPlot({
     ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$YearsInRole))+ geom_point(size = 1) +geom_smooth()+geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")+ggtitle(expression(atop("Salary Analysis_Years In Role",atop("")))) 

  })
  
## Years At Company and Salary ##  
   output$plot_YearsAtCompany <- renderPlot({
     ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$YearsAtCompany))+ geom_point(size = 1) +geom_smooth()+geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")+ggtitle(expression(atop("Salary Analysis_Years At Company",atop("")))) 

   })   

   pic_GenderAgeMoney <- ggplot(df_GenderAgeMoney, aes(x=df_GenderAgeMoney$c.rawdata.Age., y=df_GenderAgeMoney$c.rawdata.MonthlyIncome.)) + geom_point(size = 1, color=df_GenderAgeMoney$c.rawdata.Gender.+1)+ geom_smooth()
   pic_Totalworking <- ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$Totalworking))+ geom_point(size = 1) +geom_smooth() +geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")   
   pic_YearsInRole <- ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$YearsInRole))+ geom_point(size = 1) +geom_smooth()+geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")
   pic_YearsAtCompany <- ggplot(data=df_WorkingyersMonthlyincome, aes(x=df_WorkingyersMonthlyincome$MonthlyIncome, y=df_WorkingyersMonthlyincome$YearsAtCompany))+ geom_point(size = 1) +geom_smooth()+geom_vline(aes(xintercept=mean(df_WorkingyersMonthlyincome$MonthlyIncome)),col="red")
   
## Merge all Picture ##  
   output$pic_SalaryMergeAll <- renderPlot({
#     SalaryMergeAll <- ggplot2.multiplot(output$plot_GenderAgeMoney, output$plot_Totalworking ,output$plot_YearsInRole ,output$plot_YearsAtCompany ,cols=2)+ggtitle(expression(atop("Salary Analysis All",atop("")))) 
     ggplot2.multiplot(pic_GenderAgeMoney, pic_Totalworking ,pic_YearsInRole ,pic_YearsAtCompany ,cols=2)
#     return(SalaryMergeAll)
   })    
    
   
############################
## Job Satisfaction issue ##   
############################  

## Worklifebalance and Age and Distance ##
output$plot_WorklifebalanceBest <- renderPlot({
  ggplot(SubWorklifebalance_Best, aes(x=SubWorklifebalance_Best$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Best$c.rawdata.Age.))+geom_point(size=0.5, col="blue")+geom_smooth()+ggtitle(expression(atop("Worklifebalance_Best",atop("")))) 
  
})

   output$plot_WorklifebalanceBetter <- renderPlot({
     ggplot(SubWorklifebalance_Better, aes(x=SubWorklifebalance_Better$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Better$c.rawdata.Age.))+geom_point(size=0.5, col="green")+geom_smooth(col="green")+ggtitle(expression(atop("Worklifebalance_Better",atop("")))) 
     
   })
 
   output$plot_WorklifebalanceGood <- renderPlot({
     ggplot(SubWorklifebalance_Good, aes(x=SubWorklifebalance_Good$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Good$c.rawdata.Age.))+geom_point(size=0.5, col="brown")+geom_smooth(col="brown")+ggtitle(expression(atop("Worklifebalance_Good",atop("")))) 
     
   })
   
   output$plot_WorklifebalanceBad <- renderPlot({
     ggplot(SubWorklifebalance_Bad, aes(x=SubWorklifebalance_Bad$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Bad$c.rawdata.Age.))+geom_point(size=0.5, col="red")+geom_smooth(col="red")+ggtitle(expression(atop("Worklifebalance_Bad",atop("")))) 
     
   })
   
   pic_Worklifebalance_Best <- ggplot(SubWorklifebalance_Best, aes(x=SubWorklifebalance_Best$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Best$c.rawdata.Age.))+geom_point(size=0.5, col="blue")+geom_smooth()
   pic_Worklifebalance_Better <- ggplot(SubWorklifebalance_Better, aes(x=SubWorklifebalance_Better$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Better$c.rawdata.Age.))+geom_point(size=0.5, col="green")+geom_smooth(col="green")
   pic_Worklifebalance_Good <- ggplot(SubWorklifebalance_Good, aes(x=SubWorklifebalance_Good$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Good$c.rawdata.Age.))+geom_point(size=0.5, col="brown")+geom_smooth(col="brown")
   pic_Worklifebalance_Bad <- ggplot(SubWorklifebalance_Bad, aes(x=SubWorklifebalance_Bad$c.rawdata.DistanceFromHome., y=SubWorklifebalance_Bad$c.rawdata.Age.))+geom_point(size=0.5, col="red")+geom_smooth(col="red")
   
   
   ## Merge all Picture ##  
   output$pic_WorklifebalanceMergeAll <- renderPlot({
     ggplot2.multiplot(pic_Worklifebalance_Best, pic_Worklifebalance_Better ,pic_Worklifebalance_Good ,pic_Worklifebalance_Bad ,cols=2)
   })    
  
   
## Job Satisfaction and MonthlyIncome and Distance ##  
   output$plot_JobSatisfactionBest <- renderPlot({
     ggplot(SubJobSatisfaction_Best, aes(x=SubJobSatisfaction_Best$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Best$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="blue")+geom_smooth()+ggtitle(expression(atop("JobSatisfaction_Best",atop("")))) 
     
   })
   
   output$plot_JobSatisfactionBetter <- renderPlot({
     ggplot(SubJobSatisfaction_Better, aes(x=SubJobSatisfaction_Better$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Better$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="green")+geom_smooth(col="green")+ggtitle(expression(atop("JobSatisfaction_Better",atop("")))) 
     
   })
   
   output$plot_JobSatisfactionGood <- renderPlot({
     ggplot(SubJobSatisfaction_Good, aes(x=SubJobSatisfaction_Good$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Good$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="brown")+geom_smooth(col="brown")+ggtitle(expression(atop("JobSatisfaction_Good",atop("")))) 
     
   })
   
   output$plot_JobSatisfactionBad <- renderPlot({
     ggplot(SubJobSatisfaction_Bad, aes(x=SubJobSatisfaction_Bad$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Bad$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="red")+geom_smooth(col="red")+ggtitle(expression(atop("JobSatisfaction_Bad",atop("")))) 
     
   })
   
   pic_JobSatisfaction_Best <- ggplot(SubJobSatisfaction_Best, aes(x=SubJobSatisfaction_Best$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Best$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="blue")+geom_smooth()
   pic_JobSatisfaction_Better <- ggplot(SubJobSatisfaction_Better, aes(x=SubJobSatisfaction_Better$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Better$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="green")+geom_smooth(col="green")
   pic_JobSatisfaction_Good <- ggplot(SubJobSatisfaction_Good, aes(x=SubJobSatisfaction_Good$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Good$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="brown")+geom_smooth(col="brown")
   pic_JobSatisfaction_Bad <- ggplot(SubJobSatisfaction_Bad, aes(x=SubJobSatisfaction_Bad$c.rawdata.DistanceFromHome., y=SubJobSatisfaction_Bad$c.rawdata.MonthlyIncome.))+geom_point(size=0.5, col="red")+geom_smooth(col="red")
   
   
   ## Merge all Picture ##  
   output$pic_JobSatisfactionMergeAll <- renderPlot({
     ggplot2.multiplot(pic_JobSatisfaction_Best, pic_JobSatisfaction_Better ,pic_JobSatisfaction_Good ,pic_JobSatisfaction_Bad ,cols=2)
   })
   
}) 
  ## EOF
   