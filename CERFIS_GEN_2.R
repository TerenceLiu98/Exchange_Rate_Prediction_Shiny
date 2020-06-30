# Load packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(quantmod)

# Load data


############################### The UI Part ##################################

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "CERFIS Gen 2"),
    dashboardSidebar(
        sidebarMenu(
            ############### Three Menu Item #################
            menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "About", icon = icon("meh"))
            ############### Three Menu Item #################
        )
    ),
    
    dashboardBody(
        ## Tab-One: Dashboard
        tabItem(tabName = "Dashboard",
                fluidPage(
                    h3("Currency Exchange Rate Forecast Interactive System"),
                    column(12,

                    sidebarPanel(
                        textInput("forex_name_1", "Currency 1","CNY=X"),
                        verbatimTextOutput("value"),
                        dateRangeInput('dateRange',
                                       label = 'Date range input: yyyy-mm-dd',
                                       start = Sys.Date() - 30, end = Sys.Date()
                        ),
                        h5("I am Terence Lau"),
                        tags$a(href = "https://github.com/TerenceLiu98", "My GitHub"),
                        tags$br(),
                        tags$a(href = "https://terenceliu98.github.io", "My Personal Website"),
                        tags$br(),
                        # tags$br(),
                        strong("Thanks to these R package:"), code("shiny, shinydashboard, DT, quantmod")
                    ),
                    mainPanel(
                        plotOutput("plot", width = "100%")
                    ),
                )),
    )
    )
)

server <- function(input, output, session) {
    output$dateRangeText  <- renderText({
        paste("input$dateRange is", 
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    output$plot <- renderPlot(
        {
            dataset <- as.data.frame(getSymbols(input$forex_name_1, src = "yahoo",
                                   from = input$dateRange[1],to = input$dateRange[2], env = NULL))
            #dataset <- as.data.frame(Date=index(data),coredata(data))
            colnames(dataset) <-c('Open','High','Low','Close','Volume', 'Adjusted')
            candleChart(dataset, thpe = "bar", name = "CandleStick Chart", bar.type = "ohlc", theme = "white", TA = "addVo();addBBands();addCCI();addSMA();addRSI()")
            #plot <- plot_ly(dataset, x = ~Date, type="candlestick",
                                  #open = ~Open, close = ~Close,
                                  #high = ~High, low = ~Low) 
            #plot
       
        })

}


# Create Shiny object
shinyApp(ui = ui, server = server)