library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(plotly)
library(TTR)

shinyServer(function(input, output) {

    con_data <- reactive({
        file <- input$conInput
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "xlsx", "Please upload a xlsx file"))
        
        read_excel(file$datapath, col_names = c('conc'))
    })
    
    data <- reactive({
        file <- input$fileInput
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        validate(need(ext == "txt", "Please upload a txt file"))
        
        raw_data <- read_tsv(file$datapath)
        
        raw_data <- raw_data %>% 
            mutate(`J` = (`WE(1).Current (A)` * 1000000)/0.2)
        
        raw_data[raw_data$J <= 15, ]
    })
    
    #kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
    
    values <- reactiveValues(df = data.frame(time = numeric(),
                                             J = numeric()),
                             counter = 0)
    
    output$distPlot <- renderPlotly({
        plot_ly(data(), x = ~`Time (s)`, y =~J,
                type = 'scatter', mode = 'lines', name = "data")
    })
    
    #output$distPlot <- renderPlotly({
    #    dataSMA3 <- SMA(data()$J, n=5)
    #    plot_ly(data(), x = ~`Time (s)`, y =~J,
    #            type = 'scatter', mode = 'lines', name = "data") %>%
    #        add_trace(x = data()$`Time (s)`, y = dataSMA3,
    #                  type = 'scatter', mode = 'lines', name = "dataSMA3")
    #})
    
    index <- eventReactive(input$actionBtn, {
        which.min(abs(data()$`Time (s)` - strtoi(input$textInput)))
    })
    
    observeEvent(
        input$actionBtn, {
            output$contents <- renderTable({
                values$df[values$counter, 'time'] <- data()$`Time (s)`[index()]
                values$df[values$counter, 'J'] <- data()$J[index()]
                values$df
            })
            values$counter <- values$counter + 1
        }
    )
    
    lm <- reactiveValues()
    
    observeEvent(
        input$conBtn, {
            nrows <- nrow(values$df)
            output$contents <- renderTable({
                values$df['conc'] <- con_data()[1:nrows,'conc']
                values$df
            })
        }
    )
    
    output$lmSummary <- renderPrint({
        #new_df <- values$df[1:input$slider, ]
        checkbox_values <- as.numeric(input$checkbox)
        new_df <- values$df[checkbox_values, ]
        summary(lm(J ~ conc, data = new_df))
    })
    
    output$lmEquation <- renderPrint({
        checkbox_values <- as.numeric(input$checkbox)
        new_df <- values$df[checkbox_values, ]
        lm_values <- lm(J ~ conc, data=new_df)
        intercept = lm_values$coefficients['(Intercept)']
        beta = lm_values$coefficients['conc']
        paste("J =", intercept, "+", beta, "* conc")
    })
    
    output$concPlot <- renderPlot({
        checkbox_values <- as.numeric(input$checkbox)
        new_df <- values$df[checkbox_values, ]
        lm$values <- lm(J ~ conc, data=new_df)
        plot(values$df$conc, values$df$J)
        abline(lm$values, col = "blue")
    })
    
    observeEvent(input$saveBtn, {
        write.csv(values$df, paste0(input$resultsFile, '.csv'), row.names=FALSE)
    })
    
})
