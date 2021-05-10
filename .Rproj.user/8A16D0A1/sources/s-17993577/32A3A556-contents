library(tidyverse)
library(shiny)
library(shinythemes)
library(caret)

my_model <- load('/cloud/project/R/machinelearning.rda')
# DataTables example
shinyApp(
  ui = fluidPage(theme = shinytheme("cosmo"),
                 navbarPage(
                   "DiaPredict",
                   tabPanel("Ver.1.0",
                            sidebarPanel(
                              tags$h3("Input Parameters"),
                              numericInput("Var1", "Other Disease", "", "0", "1"),
                              numericInput("Var2", "Weight (Kilograms)", ""),
                              numericInput("Var3", "Height (Meters)", ""),
                              numericInput("Var4", "Family History of Type 1 Diabetes?", "", "0", "1"),
                              numericInput("Var5", "Family History of Type 2 Diabetes?", "", "0", "1"), 
                              actionButton("submitbutton", "Submit",
                                            class = "btn btn-primary")), # sidebarPanel
                            mainPanel(
                              h1("Are you likely to be Diabetic?"),
                              h2("Instructions:"),
                              h4("Select 1 for Yes or 0 for No"),
                              verbatimTextOutput("Pred"),
                              
                            ) # mainPanel
                            
                   )
                 ), # navbarPage
                 verbatimTextOutput('')
  ),
  
  server = function(input, output) {
    data_user <- eventReactive(input$submitbutton,
                               {tribble(~other_disease_binary, ~Weight, ~Height, ~FamHistT1DBin, ~FamHistT2DBin,
                                        input$Var1, input$Var2, input$Var3, input$Var4, input$Var5)})
    output$table <- renderPrint(data_user())
    
    pred <- reactive({
      predict(get(my_model),data_user())
    })
    
    output$Pred <- renderPrint(pred())
  }
)
