library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(GGally)
library(quantmod)
library(MASS)
library(fmsb)
library(RColorBrewer)
library(scales)
library(rpart)
library(rpart.plot)
library(dplyr)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(SwimmeR)



# model
source("C:/Users/anjw/Desktop/cst/cst_model.R")
model2 = train(target~., 
               data = x_train_scaled, 
               method = "ranger", 
               trControl = train_control2,
               tuneGrid = ranger_tune_grid)
model2

save(model2 , file = 'ranger.rda')


server <- function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)

  
  
  output$sample_input_data_heading = renderUI({ 
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable(striped = T, bordered = T, spacing = c("xs"), width = "auto",{    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      
      colnames(input_data) = c('region','sex','age','occp','allownc','marri_1','marri_2','DI1_dg',
                               'DI2_dg','DM1_dg','DM2_dg','DE1_dg','DC7_dg','DM8_dg','HE_BMI','LQ_1EQL',
                               'educ','BO1','BD1_11','BP1','BS3_1','L_BR_FQ','L_LN_FQ','L_DN_FQ',
                               'HE_sbp1','HE_dbp1','HE_glu','HE_HbA1c','HE_insulin','HE_chol','HE_Uacid','N_WATER'
      )
      
      input_data$HE_BMI = as.factor(input_data$HE_BMI )
      
      levels(input_data$HE_BMI) <- c("저체중","정상","비만 전단계","비만 1단계","비만 2단계","비만 3단계")
      head(input_data)
    }
  })
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        
        colnames(input_data) = c('region','sex','age','occp','allownc','marri_1','marri_2','DI1_dg',
                                 'DI2_dg','DM1_dg','DM2_dg','DE1_dg','DC7_dg','DM8_dg','HE_BMI','LQ_1EQL',
                                 'educ','BO1','BD1_11','BP1','BS3_1','L_BR_FQ','L_LN_FQ','L_DN_FQ',
                                 'HE_sbp1','HE_dbp1','HE_glu','HE_HbA1c','HE_insulin','HE_chol','HE_Uacid','N_WATER')
        
        input_data$HE_BMI = as.factor(input_data$HE_BMI )
        
        levels(input_data$HE_BMI) <- c("저체중","정상","비만 전단계","비만 1단계","비만 2단계","비만 3단계")
        
        pred_y = predict(model2,x_test_scaled) 
        result = sum(as.factor(pred_y)==y_test)/nrow(x_test_scaled)*100 #81%
        result
        
        df_pred<-cbind(x_test2, pred_y)
        df_pred<- df_pred[,-32]
        df_pred
      })
    }
  })
  
  output$tree <- renderPrint({
    rpart(y_train ~ .,
          data = xx,
          control = rpart.control(
            minsplit = 1, 
            minbucket = 1, 
            cp=0.01))
  })
  
  output$treeplot <- renderPlot({
    rpart.plot(rpart(y_train ~ .,
                     data = xx))
  })
  
  output$table3 <- renderTable({
    sbp = c(as.numeric(input$sbp))
    dbp = c(as.numeric(input$dbp))
    glu =c(as.numeric(input$glu))
    water =c(as.numeric(input$water))
    age =c(as.numeric(input$age))
    table <- data.frame(sbp, dbp,
                        glu, water,
                        age)
  })
  
  table1 <- reactive({
    sbp = as.numeric(input$sbp)
    dbp = as.numeric(input$dbp)
    glu =as.numeric(input$glu)
    water=as.numeric(input$water)
    age=as.numeric(input$age)
    
    table1 <- data.frame(age, sbp,
                         dbp, glu,
                         water)
  })
  
  output$distplot <- renderPlot({
    #sbp = as.numeric(input$sbp)
    #dbp = as.numeric(input$dbp)
    #glu =as.numeric(input$glu)
    #water=as.numeric(input$water)
    #age=as.numeric(input$age)
    
    #table1 <- data.frame(sbp, dbp,
    #                     glu, water,
    #                     age)
    table1 <- table1()
    for(i in 1:5){
      for(j in 1){
        table1[j,i] <- (table1[j,i]-min(x_train2[,i])) / (max(x_train2[,i])-min(x_train2[,i]))
      }
    }
    table1 <- rbind(x_train3,table1)
    table1 <- rbind(rep(max(table1),5),rep(min(table1),5),table1)
    theGraph <- radarchart(table1)
    print(theGraph)
    
  })
  
  output$sample_prediction_heading = renderUI({  
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable(striped = T, bordered = T, spacing = c("xs"), width = "auto",{   
    pred = predictions()
    head(pred)
  })
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
}

ui <- fluidPage(
  titlePanel("Obesity prediction app"),
  navbarPage("LIST", theme = shinytheme("cerulean"),
             tabPanel("data",fluid = TRUE, icon = icon("upload"),
                      br(),
                      br(),
                      br(),
                      br(),
                      tags$h4("Note: All BMI information is based on the Korea National Health & Nutrition Examination Survey. This data does not collect personal information.", style="font-size:150%"),
                      
                      
                      br(),
                      
                      tags$h4("To predict using this model, upload test data in csv format by using the button below.", style="font-size:150%"),
                      
                      tags$h4("Then, go to the", tags$span("Download Predictions",style="color:red"),
                              tags$span("section in the sidebar to download the predictions."), style="font-size:150%"),
                      
                      br(),
                      br(),
                      br(),
                      column(width = 4,
                             fileInput('file1', em('Upload test data in csv format ',style="text-align:center;font-size:150%"),multiple = FALSE,
                                       accept=c('.csv')),
                             
                             uiOutput("sample_input_data_heading"),
                             tableOutput("sample_input_data"),
                             
                             
                             br(),
                             br(),
                             br(),
                             br()
                      ),
                      br()
             ),
             tabPanel("download",fluid = TRUE, icon = icon("download"),
                      fluidRow(
                        br(),
                        br(),
                        br(),
                        br(),
                        column(width = 8,
                               tags$h4("After you upload a dataset, you can download the predictions in csv format by
                                    clicking the button below.", 
                                    style="font-size:150%"),
                               br(),
                               br()
                        )),
                      fluidRow(
                        column(width = 4,
                               uiOutput("sample_prediction_heading"),
                               tableOutput("sample_predictions")),
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
                        column(width = 7,
                               downloadButton("downloadData", em('File Download',style="text-align:center;font-size:150%")),
                               plotOutput('plot_predictions')
                        )
                      )
             ),
             tabPanel("Tree Model",fluid = TRUE, icon = icon("map"),
                      mainPanel(
                        verbatimTextOutput("tree"),
                        plotOutput("treeplot")
                      )
                      
             ),
             tabPanel("check",fluid = TRUE, icon = icon("chart-bar"),
                      tags$h4("Please write it down in the question section below.", style="font-size:150%"),
                      div(style="display: inline-block;vertical-align:top; width: 500px;",
                          strong("만나이"), 
                          textInput("age", NULL, width = 100)),
                      div(style="display: inline-block;vertical-align:top; width: 500px;",
                          strong("수축기 혈압"), 
                          textInput("sbp", NULL, width = 100)),
                      
                      div(style="display: inline-block;vertical-align:top; width: 500px;",
                          strong("이완기 혈압"), 
                          textInput("dbp", NULL, width = 100)),
                      
                      div(style="display: inline-block;vertical-align:top; width: 500px;",
                          strong("공복혈당"),
                          textInput("glu", NULL, width = 100)), 
                      
                      div(style="display: inline-block;vertical-align:top; width: 500px;",
                          strong("수분섭취량"),
                          textInput("water", NULL, width = 100)),
                      br(),
                      actionButton(inputId = "label",label="update"),
                      br(),
                      tableOutput('table3'),
                      plotOutput('distplot')
             )
             
  ))

shinyApp(ui,server)

library(rsconnect)
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/rsconnect")
rsconnect::setAccountInfo(name="ggongji", token="D298D874371F732A14039D50A634872D", secret="xDBLS0lOwx9xVgVbnnK8XQuW3TbiRjmqANOEOA60")
