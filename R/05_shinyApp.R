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

my_model <- load('/cloud/project/machinelearning.rda')

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
                             numericInput("txt4", "Family history of type 1 diabetes?", ""),
                             numericInput("txt5", "Family history of type 2 diabetes?", "")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             textOutput("Pred"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  data <- reactive({
    tribble(genderBin=input$txt1,
               Weight=input$txt2,
               Height=input$txt3,
               FamHistT1DBin=input$txt4,
               FamHistT2DBin=input$txt5,
               )
  })
  
  pred <- reactive({
    predict(my_model,data())
  })
  
  output$Pred <- renderText(pred())
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)