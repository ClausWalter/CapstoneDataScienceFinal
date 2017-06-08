#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Word Prediction Algorithm"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        textInput("txtInput", "Enter your phrase here", "Enter phrase for prediction", width='300px'),
                        br(),
                        actionButton("btnPredict", "Predict", width='150px'),
                        br(),
                        p(br(), "Click to get prediction"),
                        br(),
                        actionButton("btnTake", "Take over", width='150px'),
                        br(),
                        p(br(), "Click to take over prediction")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(id="WordPrediction", type="tabs",
                                tabPanel(title = "Word Prediction", value = "predictionInput",
                                         p("Best fit:"),
                                         verbatimTextOutput("predictionText1", placeholder=TRUE)),
                                tabPanel("Prediction Details and History",dataTableOutput("dfStatistics")),
                                tabPanel("How to use", includeHTML("How_to.html")),
                                tabPanel("Advanced Documentation", includeHTML("Technical_Documentation.html")),
                                tabPanel("2-gram",  dataTableOutput("dfNgram2")),
                                tabPanel("3-gram",  dataTableOutput("dfNgram3")),
                                tabPanel("4-gram",  dataTableOutput("dfNgram4")),
                                tabPanel("5-gram",  dataTableOutput("dfNgram5")),
                                tabPanel("2-gram Personal",  dataTableOutput("dfNgram2Personal")),
                                tabPanel("3-gram Personal",  dataTableOutput("dfNgram3Personal")),
                                tabPanel("4-gram Personal",  dataTableOutput("dfNgram4Personal"))
                                
                        )
                )
        )
))
