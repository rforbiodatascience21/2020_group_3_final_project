####################################
#    R for Bio Data ScienceGroup3  #
#              Group 3             #
####################################

#this is a comment

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(caret)

load('/cloud/project/machinelearning.rda')

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "My first app",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("txt1", "Gender: (1 = female, 0 = male)", ""),
                             numericInput("txt2", "Weight in kilograms:", ""),
                             numericInput("txt3", "Height in meters:", ""),
                             numericInput("txt4", "Family history of type 1 diabetes? (1 = yes, 0 = no)", ""),
                             numericInput("txt5", "Family history of type 2 diabetes? (1 = yes, 0 = no)", ""),
                             actionButton("enter", label = "Enter Values")
                           ), # sidebarPanel
              
                             mainPanel(renderTable("Pred"))
                           
                            # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
<<<<<<< HEAD
server <- function(input, output, session) 
  {
  new_data <- eventReactive(
    input$enter, 
    {
    tribble(~genderBin, ~Weight, ~Height, ~FamHistT1DBin, ~FamHistT2DBin, 
           input$txt1, input$txt2, input$txt3, input$txt4, input$txt5)
=======
server <- function(input, output) {
  data <- reactive({
    req(input$genderBin)
    data.frame(genderBin=input$txt1,
               Weight=input$txt2,
               Height=input$txt3,
               FamHistT1DBin=input$txt4,
               FamHistT2DBin=input$txt5,
               )
>>>>>>> 19290217ac6bee92e68828c52bb07f84f3b47706
  })
  
 pred <- reactive({
   predict(final_model, new_data())
   withProgress(message = 'Predictions in progress. Please wait ...')
 })
  
  output$Pred <- renderTable(pred())
 }
# server


# Create Shiny object
shinyApp(ui = ui, server = server)