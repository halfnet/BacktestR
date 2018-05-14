source('BacktestMain.R', local = TRUE)

library(shiny)

ui <- tagList(
    tags$head(
        tags$script(
            HTML("
                 Shiny.addCustomMessageHandler ('print',function (message) {
                 $('#'+message.selector).html(message.html);
                 console.log(message);
                 });
                 ")
            )
        ),       
    navbarPage(title = "Backtest",
      tabPanel(title = "Main",
               fluidRow(
                   column(1, 
                          actionButton(inputId = "go", 
                                       label = "GO!")
                          ),
                   column(1, 
                          actionButton(inputId = "save", 
                                       label = "SAVE")
                   ),
                   column(10, 
                          textOutput("status")
                   )
               ),
               fluidRow(
                   tags$hr()
               ),
               fluidRow(
                   column(4,
                          helpText("Company Score Parameters:"),
                          checkboxInput(inputId = "CS.UseFile", 
                                        label = "Use Saved File?",
                                        value = TRUE),
                          textInput(inputId = "CompanyScoreFolder", 
                                    label = "Company Score Folder",
                                    value = "Company scores/"),
                          numericInput(inputId = "CS.MaxRange.Q", 
                                       label = "Max Range When RF = Quarter",
                                       value = -250),
                          numericInput(inputId = "CS.MaxRange.SA", 
                                       label = "Max Range When RF = Semi-Annual",
                                       value = -380),
                          numericInput(inputId = "CS.AdjEntryDate.123Q", 
                                       label = "Adjusted Entry Date for Quarters 1-3",
                                       value = 100),
                          numericInput(inputId = "CS.AdjEntryDate.1S", 
                                       label = "Adjusted Entry Date for first half",
                                       value = 100),
                          numericInput(inputId = "CS.AdjEntryDate.4Q", 
                                       label = "Adjusted Entry Date for Quarter 4",
                                       value = 160),
                          numericInput(inputId = "CS.AdjEntryDate.2S", 
                                       label = "Adjusted Entry Date for second half",
                                       value = 160),
                          numericInput(inputId = "CS.Opinion.NA", 
                                       label = "Maximum NA for inclusion",
                                       value = 2),
                          numericInput(inputId = "CS.TableOption", 
                                       label = "Table Option",
                                       value = 160)
                   ),
                   column(4, 
                          helpText("Model Construction Parameters:"),
                          checkboxInput(inputId = "ConstructModelPort", 
                                        label = "Construct Model Portfolio?",
                                        value = TRUE),
                          selectInput(inputId = "ConstructModelPortMethod", 
                                      label = "Construct Model Port Method:",
                                      c("MV%" = 1,
                                        "DtS" = 2,
                                        "DtS Cash" = 3)),
                          textInput(inputId = "RootFolder", 
                                    label = "Root Folder",
                                    value = "C:/MyProjects/Guru/EMBackTesting/Backtest_000/"),
                          textInput(inputId = "IndexDataFolder", 
                                    label = "Index Data Folder",
                                    value = "Bond raw data/Index constituents/"),
                          numericInput(inputId = "OutPerformMultiple", 
                                       label = "Outperform Multiple",
                                       value = 3),
                          numericInput(inputId = "MaxWeightPerName", 
                                       label = "Max Weight Per Name (%)",
                                       value = 5),
                          numericInput(inputId = "BuyM.RatioCutOff", 
                                       label = "Buy M-11 Ratio CutOff",
                                       value = 0),
                          numericInput(inputId = "HoldM.RatioCutOff", 
                                       label = "Hold M-11 Ratio CutOff",
                                       value = 1),
                          numericInput(inputId = "ConstructModelPortDtSPHold", 
                                       label = "DtS Percentile Hold",
                                       value = 0.5),
                          numericInput(inputId = "ConstructModelPortDtSPBuy", 
                                       label = "DtS Percentile Buy",
                                       value = 0.9),
                          numericInput(inputId = "TScoreHoldFilter", 
                                       label = "TScore Hold Filter (0=no filter)",
                                       value = 0),
                          numericInput(inputId = "TScoreBuyFilter", 
                                       label = "TScore Buy Filter (0=no filter)",
                                       value = 0),
                          numericInput(inputId = "TScoreCutOff", 
                                       label = "TScore CutOff",
                                       value = -1),
                          numericInput(inputId = "TScoreNA", 
                                       label = "TScore NA",
                                       value = -1),
                          textInput(inputId = "Ind_Lvl1", 
                                    label = "Industry Level 1 Filter",
                                    value = "ALL"),
                          textInput(inputId = "Ind_Lvl2", 
                                    label = "Industry Level 2 Filter",
                                    value = "ALL"),
                          textInput(inputId = "Ind_Lvl3", 
                                    label = "Industry Level 3 Filter",
                                    value = "ALL"),
                          textInput(inputId = "Ind_Lvl4", 
                                    label = "Industry Level 4 Filter",
                                    value = "ALL")
                          
                   ),
                   column(4, 
                          helpText("Threshold Parameters:"),
                          checkboxInput(inputId = "TH.UseFile", 
                                        label = "Use Saved File?",
                                        value = TRUE),
                          
                          numericInput(inputId = "TH.MinWght", 
                                       label = "Minimum Portfolio Weight (%)",
                                       value = 50)
                   )
               ),
               fluidRow(
                   tags$hr()
               ),
               fluidRow(
                   tableOutput('show_inputs')
               )
               
      ),
      tabPanel(title = "Current Holdings",
               fluidRow(
                   column(1, 
                          actionButton(inputId = "CH_go", 
                                       label = "GO!")
                   ),
                   column(10, 
                          textOutput("CH_status")
                   )
               ),
               fluidRow(
                   tags$hr()
               ),
               fluidRow(
                   column(4,
                          helpText("Company Score Parameters:"),
                          checkboxInput(inputId = "CH_CS.UseFile", 
                                        label = "Use Saved File?",
                                        value = TRUE)
                   ),
                   column(4, 
                          helpText("Model Construction Parameters:"),
                          checkboxInput(inputId = "CH_ConstructModelPort", 
                                        label = "Construct Model Portfolio?",
                                        value = TRUE),
                          selectInput(inputId = "CH_ConstructModelPortMethod", 
                                      label = "Construct Model Port Method:",
                                      c("MV%" = 1,
                                        "DtS" = 2,
                                        "DtS Cash" = 3)),
                          dateInput("CH_OpinionDate", "Opinion Date:")                          
                   ),
                   column(4, 
                          helpText("Threshold Parameters:"),
                          checkboxInput(inputId = "CH_TH.UseFile", 
                                        label = "Use Saved File?",
                                        value = FALSE)
                          
                   )
                   

            )
      ),
      navbarMenu(title = "Other",
        tabPanel(title = "Sub Menu1",
             plotOutput("chisq"),
             actionButton("rechisq", "Resample")
        ),
        tabPanel(title = "Sub Menu2",
             plotOutput("norm"),
             actionButton("renorm", "Resample")
        )
      )
    )
)

server <- function(input, output, session) {
  
    params = read.csv("parameters.csv")
    for(i in 1:nrow(params)) {
        row = params[i,]
        updateTextInput(session, row$name, value = row$value)
    }
    TH.Limits = read.csv("indices.csv")
    
    rv <- reactiveValues(
        status = "ready",
        CH_status = "ready",
        norm = rnorm(500), 
        unif = runif(500),
        chisq = rchisq(500, 2))

    observeEvent(input$go, {
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "processing..."))
        processData(input$RootFolder, input$CS.UseFile, input$TH.UseFile, input$ConstructModelPort, input$ConstructModelPortMethod, FALSE, NULL) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "done!"))
    })
    
    observeEvent(input$CH_go, {
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = "processing..."))
        processData(input$RootFolder, input$CH_CS.UseFile, input$CH_TH.UseFile, input$CH_ConstructModelPort, input$CH_ConstructModelPortMethod, TRUE, input$CH_OpinionDate) 
        session$sendCustomMessage(type = 'print', message = list(selector = 'CH_status', html = "done!"))
    })
    
    output$status <- renderText({rv$status})
    output$CH_status <- renderText({rv$CH_status})
    
    observeEvent(input$save, {
        df = AllInputs()
        params$value = df[match(params$name, df$name),2]
        write.table(params, file = "parameters.csv", sep = ",", col.names = TRUE, row.names = FALSE)
        session$sendCustomMessage(type = 'print', message = list(selector = 'status', html = "saved!"))
    })
    
    output$show_inputs <- renderTable({
        TH.Limits
    })    
    
    AllInputs <- reactive({
        x <- reactiveValuesToList(input)
        data.frame(
            name = names(x),
            value = unlist(x, use.names = FALSE)
        )
    })
    
    
    
    observeEvent(input$renorm, { rv$norm <- rnorm(500) })
    observeEvent(input$reunif, { rv$unif <- runif(500) })
    observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })

    
  output$norm <- renderPlot({
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
      main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
      main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
      hist(rv$chisq, breaks = 30, col = "grey", border = "white",
           main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
  
}

shinyApp(server = server, ui = ui)