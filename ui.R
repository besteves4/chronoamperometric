library(shiny)
library(plotly)

shinyUI(fluidPage(

    # Application title
    titlePanel(""),

    sidebarLayout(
        sidebarPanel(
            fileInput("fileInput", "Choose data file:", accept = ".txt"),
            textInput("textInput", "Time:"),
            actionButton("actionBtn", "Add J value", class = "btn-success"),
            fileInput("conInput", "Choose concentration file:"),
            actionButton("conBtn", "Add concentration values", class = "btn-success"),
            #sliderInput("slider", "Choose number of points for regression:", min = 3, max = 7, value = 5, step = 1),
            checkboxGroupInput("checkbox", "Choose points for regression:",
                               inline = TRUE,
                               choices = c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                                           10, 11, 12, 13, 14, 15, 16, 17),
                               selected = c(1, 2, 3, 4, 5, 6, 7, 8, 9,
                                           10, 11, 12, 13, 14, 15, 16, 17)),
            textInput("resultsFile", "Save results as:"),
            actionButton("saveBtn", "Save results", class = "btn-info"),
        ),
        mainPanel(
            fluidRow(column(width = 12,
                            plotlyOutput('distPlot'))),
            fluidRow(column(width = 4, offset = 1,
                            tableOutput("contents")),
                     column(width = 6,
                            verbatimTextOutput("lmSummary"))),
            fluidRow(column(width = 10, offset = 1, 
                            verbatimTextOutput("lmEquation"))),
            fluidRow(column(width = 12, 
                            plotOutput("concPlot")))
        )
    )
))
