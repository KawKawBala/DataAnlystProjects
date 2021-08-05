#install.packages("shiny","dplyr","reactable","shinydashboard","tidyverse")
#install.packages("flexdashboard")
#install.packages("randomForest")
#install.packages("ipred")
#install.packages("ISLR")
#install.packages("tree")
#install.packages("shinyWidgets")
#install.packages('rsconnect')
#install.packages('caret', dependencies = TRUE)
#install.packages('e1071', dependencies=TRUE)

library(ISLR)
library(tree)
library(e1071)
library(shiny)
library(tidyverse)
library(dplyr)
library(reactable)
library(shinydashboard)
library(shinyWidgets)
library(flexdashboard)
library(reactable)
library(stringr)
library(caret)
library(mlbench)
library(rpart)
library(ipred)
library(randomForest)
library(rsconnect)

# Load Data Portuguese Language
Import_Math<-read.csv(file="Export_Math.csv")
Import_Port<-read.csv(file="Export_Port.csv")
#Math_Pred<-readRDS("Math_Pred.rda")
#Port_Pred<-readRDS("Port_Pred.rda")

Import_Math<- Import_Math %>% select(-c(X))
Import_Port<- Import_Port %>% select(-c(X))



ui <- dashboardPage(
  
  
  # Application title
  dashboardHeader(title="Student Performance Software"),
  
  
  
  # Dashboard Sidebar 
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
    menuItem("Portuguese", tabName = "Port", icon = icon("table")),
    menuItem("Mathematics", tabName = "Math", icon = icon("table")),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    h6("Tool created by @mizquierdo")
    
    
  )),
  
  
  
  dashboardBody(
    tabItems( 
      
      #Tab 1
      tabItem(tabName = "Intro",
             h3 ( "This tool has been created with the capability of Predicting the Final Grades 
                        for the Students of Mathematics and Portuguese Languange subjects of schools
                        : Gabriel Pereira and Mousinho da Silveira."),
             h3("Data has been extracted from public UCI Machine Learning repository (https://archive.ics.uci.edu/ml/datasets/student+performance),
                cleaned and reduced to the best predictive academic varaibles available to academic professionals."),
            h3("The predictive model used is Random Forest for both Mathematics and Portuguese Language subjects with an accuracy from
               35% without previous grades and up to +80% when previous grading are available."),
            h2("Instructions: "), 
                         h3("  - Select and input the student information"),
                         h3("  - Click on Enter button"),
                         h3("  - The predicted group will show in a few seconds"),
                         h3("  - Options can be: High Pass(17<20) - Low Pass(10<16) - Low Fail(7<9) - High Fail(0<6)")
                 
              
              
              
      ),
      
      #Tab 2
      tabItem(tabName = "Port",
              fluidRow(
             box( radioButtons("Gender","Gender",choices = c("F", "M")), width = 2, height = 110),
                       
             box( selectInput("Age","Age",choices = 15:18, width = 100),width = 2, height = 110),
             
             box( selectInput("Studytime","Studytime",choices = 1:4, width=100),width = 2, height = 110),
             
             box(radioButtons("Familysupport", "Family Support",choices =  c("yes", "no")),width = 2, height = 110),
             
             box(radioButtons("Activities", "Activities",choices =  c("yes", "no")),width = 2, height = 110),
              
             box(radioButtons("Paidclasses", "Extra Paid Classes", choices =  c("yes", "no")),width = 2, height = 110)
              ),
             fluidRow(
              
              
               box( selectInput("Grade1", "1st Examination Grade", choices=1:20, selected = 10,width = 100),width = 2, height = 110),
              
               box( selectInput("Grade2", "2nd Examination Grade", choices=1:20,selected = 10, width = 100),width = 2, height = 110),
               
               box(numericInput("Absences", "Course Absences:", value = 1,min = 0,max = 150, width = 100),width = 2, height = 110),
               
               box( selectInput("Failures","Previous Fails",choices = 0:3, width = 100),width = 2, height = 110),
             
               box( radioButtons("Schoolsupport", "School Support",choices =  c("yes", "no")),width = 2, height = 110),
               
               box( radioButtons("HigherEducation", "Higher Education",choices =  c("yes", "no")),width = 2, height = 110),
               
               
              #radioButtons("Nursery","Nursery",choices = c("Yes", "No")),
              
              #radioButtons("Romantic", "In a Relationship", choices =  c("Yes", "No")),
              
              #selectInput("Free Time", "Free Time", choices=1:5),
              
              #selectInput("Go Out", "Going Out with Friends", choices=1:5),
              
              #selectInput("Weekend Alcohol", "Weekend Alcohol", choices=1:5),
              
              #selectInput("Week Alcohol", "During Week Alcohol", choices=1:5),
              
              actionButton("Enter", "Enter",style="padding:50px; font-size:120%;float:right ")
             ),
             
               # Show results
             
             #tableOutput("Student2"),
             #textOutput("Student"),
             
             infoBoxOutput("modelSummary", width = NULL)
             
             
             
             
             
             
      
            
      ),
  
      #Tab 3
      tabItem(tabName = "Math",
              fluidRow(
                box( radioButtons("Gender","Gender",choices = c("F", "M")), width = 2, height = 110),
                
                box( selectInput("Age","Age",choices = 15:18, width = 100),width = 2, height = 110),
                
                box( selectInput("Studytime","Studytime",choices = 1:4, width=100),width = 2, height = 110),
                
                box(radioButtons("Familysupport", "Family Support",choices =  c("yes", "no")),width = 2, height = 110),
                
                box(radioButtons("Activities", "Activities",choices =  c("yes", "no")),width = 2, height = 110),
                
                box(radioButtons("Paidclasses", "Extra Paid Classes", choices =  c("yes", "no")),width = 2, height = 110)
              ),
              fluidRow(
                
                
                box( selectInput("Grade1", "1st Examination Grade", choices=1:20, selected = 10,width = 100),width = 2, height = 110),
                
                box( selectInput("Grade2", "2nd Examination Grade", choices=1:20,selected = 10, width = 100),width = 2, height = 110),
                
                box(numericInput("Absences", "Course Absences:", value = 1,min = 0,max = 150, width = 100),width = 2, height = 110),
                
                box( selectInput("Failures","Previous Fails",choices = 0:3, width = 100),width = 2, height = 110),
                
                box( radioButtons("Schoolsupport", "School Support",choices =  c("yes", "no")),width = 2, height = 110),
                
                box( radioButtons("HigherEducation", "Higher Education",choices =  c("yes", "no")),width = 2, height = 110),
                
                
                #radioButtons("Nursery","Nursery",choices = c("Yes", "No")),
                
                #radioButtons("Romantic", "In a Relationship", choices =  c("Yes", "No")),
                
                #selectInput("Free Time", "Free Time", choices=1:5),
                
                #selectInput("Go Out", "Going Out with Friends", choices=1:5),
                
                #selectInput("Weekend Alcohol", "Weekend Alcohol", choices=1:5),
                
                #selectInput("Week Alcohol", "During Week Alcohol", choices=1:5),
                
                actionButton("Enter2", "Enter",style="padding:50px; font-size:120%;float:right ")
              ),
      
              # Show results
      
              infoBoxOutput("modelSummary2", width = NULL) 
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
 
#Portuguese   
  observeEvent(input$Enter, {
    sex=as.character(input$Gender)
    age=as.numeric(input$Age)
    studytime.y=as.numeric(input$Studytime)
    famsup.y=as.character(input$Familysupport)
    activities.y=as.character(input$Activities)
    paid.y=as.character(input$Paidclasses)
    G1.y=as.numeric(input$Grade1)
    G2.y=as.numeric(input$Grade2)
    absences.y=as.numeric(input$Absences)
    failures.y=as.numeric(input$Failures)
    schoolsup.y=as.character(input$Schoolsupport)
    higher.y=as.character(input$HigherEducation)
    
    #t <- data.frame(as.factor(input$Gender),input$Age, as.factor(input$Familysupport), as.factor(input$Studytime),
     #               as.factor(input$Activities), as.factor(input$Paidclasses),input$Grade1, input$Grade2,input$Absences,
     #               as.factor(input$Failures), as.factor(input$Schoolsupport), as.factor(input$HigherEducation))
    
    z<-data.frame(age,sex,G1.y,G2.y,absences.y, failures.y,schoolsup.y,paid.y,famsup.y,activities.y,studytime.y,
                                higher.y)
    
    #train_Port <- sample(1:nrow(Import_Port), 296)
    #tree.port <- tree(G3.y~., data=Import_Port, subset = train_Port)
    #summary(tree.port)
    
    #RF.Port <- randomForest(G3.y~., Import_Port,subset=train_Port)
    
    
    inTrain = createDataPartition(y = Import_Port$G3.y, p = .80, list = FALSE)
    training = Import_Port[inTrain,]
    testing = Import_Port[-inTrain,] 
    
    fit.rf <- train(G3.y~., data=training, method="rf")
    
        output$modelSummary <- renderInfoBox({
          infoBox("Predicted Final Grade for Portuguese Language", predict(fit.rf, newdata= z),  icon = icon("user-graduate"),color = "blue") 
              })
  })
  
# Mathematics 
  
  observeEvent(input$Enter2, {
    sex=as.character(input$Gender)
    age=as.numeric(input$Age)
    studytime.x=as.numeric(input$Studytime)
    famsup.x=as.character(input$Familysupport)
    activities.x=as.character(input$Activities)
    paid.x=as.character(input$Paidclasses)
    G1.x=as.numeric(input$Grade1)
    G2.x=as.numeric(input$Grade2)
    absences.x=as.numeric(input$Absences)
    failures.x=as.numeric(input$Failures)
    schoolsup.x=as.character(input$Schoolsupport)
    higher.x=as.character(input$HigherEducation)
  
    z_Math<-data.frame(age,sex,G1.x,G2.x,absences.x, failures.x,schoolsup.x,paid.x,famsup.x,activities.x,studytime.x,
                  higher.x)
    
    
    
    inTrain_Math = createDataPartition(y = Import_Math$G3.x, p = .80, list = FALSE)
    training_Math = Import_Math[inTrain,]
    testing_Math = Import_Math[-inTrain,] 
    
    fit.rf.Math <- train(G3.x~., data=training_Math, method="rf")
    
    output$modelSummary2 <- renderInfoBox({
      infoBox("Predicted Final Grade for Mathematics", predict(fit.rf.Math, newdata= z_Math), icon = icon("user-graduate"),color = "blue") 
    })
  })
  
 
  
 

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
