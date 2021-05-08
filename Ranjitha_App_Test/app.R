####################################
#    R for Bio Data ScienceGroup3  #
#              Group 3             #
####################################

#this is a comment

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)


my_model <- load('/cloud/project/machinelearning.rda')

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                    
                    "DiaPredict",
                    tabPanel("Ver1.0",
                             sidebarPanel(
                                 tags$h3("Input Parameters"),
                                 numericInput("int1", "Gender (1 = Female, 0 = Male)", "", "0", "1"),
                                 numericInput("int2", "Weight (Kilograms)", "", "0"),
                                 numericInput("int3", "Height (Meters)", "", "0"),
                                 numericInput("int4", "Family History of Type 1 Diabetes?", "", "0", "1"),
                                 numericInput("int5", "Family History of Type 2 Diabetes?", "", "0", "1"),
                                 
                                 actionButton("submitbutton", "Submit", 
                                              class = "btn btn-primary")
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Are you likely to be Diabetic?"),
                                 
                                 textOutput('Pred')
                                 
                             ))))
# fluidPage


# Define server function  
server <- function(input, output) {
    data <- reactive({
     req(input$genderBin)
        data.frame(genderBin=input$txt1,
                   Weight=input$txt2,
                   Height=input$txt3,
                   FamHistT1DBin=input$txt4,
                   FamHistT2DBin=input$txt5)
                })
    
    
    pred <- reactive({
        predict(get(my_model),data)
    })
    
    output$Pred <- renderText(pred())
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)