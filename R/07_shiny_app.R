library(tidyverse)
library(shiny)
library(shinythemes)
library(caret)

my_model <- load('/cloud/project/results/machinelearning.rda')
# DataTables example
shinyApp(
  ui = fluidPage(theme = shinytheme("cerulean"),
                 navbarPage(
                   # theme = "cerulean",  # <--- To use a theme, uncomment this
                   "My first app",
                   tabPanel("Navbar 1",
                            sidebarPanel(
                              tags$h3("Input:"),
                              numericInput("Var1", "Gender: (1 = female, 0 = male)", ""),
                              numericInput("Var2", "Weight in kilograms:", ""),
                              numericInput("Var3", "Height in meters:", ""),
                              numericInput("Var4", "Family history of type 1 diabetes?", ""),
                              numericInput("Var5", "Family history of type 2 diabetes?", "")
                            ), # sidebarPanel
                            mainPanel(
                              h1("Header 1"),
                              
                              h4("Output 1"),
                              dataTableOutput("Pred"),
                              
                            ) # mainPanel
                            
                   )
                 ), # navbarPage
                 dataTableOutput('table')
  ),
  
  server = function(input, output) {
    data_user <- reactive({tribble(~genderBin, ~Weight, ~Height, ~FamHistT1DBin, ~FamHistT2DBin,
                                   input$Var1, input$Var2, input$Var3, input$Var4, input$Var5)})
    output$table <- renderDataTable(data_user(),
                                    options = list(searching = FALSE),
    )
    pred <- reactive({
      predict(get(my_model),new_data = data_user())
    })
    
    output$Pred <- renderDataTable(pred())
  }
)
