## app.R ##
library(shiny)
library(shinydashboard)
library(DT)
library(quantmod)

# source("./libraries.R")
# source("./functions.R")

############### K Line Curve ########################

# FT_Kplot<-function(OPEN,HIGH,LOW,CLOSE,DATE)
# {
#   N<-length(OPEN)
#   w<-0.3
#   D<-CLOSE-OPEN
#   par(family='serif')
#   # 作图
#   plot(c(1:N),CLOSE,type='n',xaxt='n',
#        xlab='Time',ylab='Price',font.axis=1.5)
#   title(main='K Curve',cex=2,col='black')
#   for(i in 1:N){
#     lines(c(i,i),c(LOW[i],HIGH[i]),col='black',lwd=1)
#     x<-c(i-w,i-w,i+w,i+w)
#     y<-c(OPEN[i],CLOSE[i],CLOSE[i],OPEN[i])
#     if(D[i]<0)
#     {
#       polygon(x,y,col='green',border='green')
#     } else
#     {
#       polygon(x,y,col='red',border='red')
#     }
#   }
#   Index<-seq(from=1,to=N,length=5)
#   Index<-round(Index)
#   Text<-DATE[Index]
#   axis(side=1,Index,labels=Text,cex.axis=1)
# }

################################################



# INR <- read.csv("USD_INR.csv", header = T)
# INR$Date <- as.Date(as.character(INR$Date), format = '%B %d, %Y')
# 
# CNY <- read.csv("USD_CNY.csv", header = T)
# CNY$Date <- as.Date(as.character(CNY$Date), format = '%B %d, %Y')

################# Update Latest Data & Data Preprocessing ###################
getFX("USD/INR",src = "yahoo",from = as.Date(as.numeric(Sys.Date()) - 56),to = Sys.Date())
getFX("USD/CNY",src = "yahoo",from = as.Date(as.numeric(Sys.Date()) - 56),to = Sys.Date())

write.zoo(USDINR, file = "INR.csv",  sep = ",", qmethod = "double")
write.zoo(USDCNY, file = "CNY.csv",  sep = ",", qmethod = "double")

INR <- read.csv("INR.csv", header = T)
colnames(INR)[1] <- 'Date'
colnames(INR)[2] <- 'Price'

CNY <- read.csv("CNY.csv", header = T)
colnames(CNY)[1] <- 'Date'
colnames(CNY)[2] <- 'Price'

#############################################################################



############################### The UI Part ##################################
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "CERFIS"),
  dashboardSidebar(
    sidebarMenu(
      ############### Three Menu Item #################
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Dataset", tabName = "Dataset", icon = icon("database")),
      menuItem("About", tabName = "About", icon = icon("meh"))
      ############### Three Menu Item #################
    )
  ),
  ############### Body content #################
  dashboardBody(
    
    tabItems(
      ####### First tab content ##########
      tabItem(tabName = "dashboard",
              fluidPage(
                h3("Currency Exchange Rate Forecast Interactive System"),
                tags$strong("Step One: Input your data"),
                tags$article("On the Sidebar tag:Dataset, we have prepared the File for you to run "),
                tags$br(),
                tags$br(),
                tags$strong("Step Two: adjust the time interval"),
                tags$article("choosing the right inveral is the key to get a good forecast, recommended value is one to two month")
              ),
                tags$br(),
                tags$br(),
              column(8,
              fileInput("inputdata", "Choose You CSV File",
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
              ),
              checkboxInput("header", "Header", TRUE),
              tags$br(),
              # tags$strong("The Dataset must contain Date and Price"),
              tags$br(),
                #box(status = "warning", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 500, width = "150%"))
                  plotOutput("plot1", width = "100%")
                # Copy the line below to make a text input box
                # Copy the line below to make a text input box
              ),
              
              ############# Date Widget ############
              # column(4,
              #   dateInput("date1", "Date:", value = "2018-08-10"),
              #   dateInput("date2", "Date:", value = "2019-08-09"),
              #   submitButton(text = "Submit")
              # ),
              ############# Date Widget ############
              column(4, 
                     tags$br(),
                     tags$br(),
                     tags$br(),
                titlePanel("Forcesting"),
                # textInput("Value1", "Day One", 3.23),
                # textInput("Value2", "Day Two", 3.20),
                # textInput("Value3", "Day Three", 3.25),
                # submitButton(text = "Submit"),
                textOutput("result1", inline = TRUE)
              )
              ),
      tabItem(tabName = "Dataset",
              fluidPage(
                
                # App title ----
                titlePanel("Downloading Data"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Choose dataset ----
                    selectInput("dataset", "Choose a dataset:",
                                choices = c("USD_INR", "USD_CNY")),
                    
                    # Button
                    downloadButton("downloadData", "Download"),
                    tags$article("Data source:"),
                    tags$a(href = "https://finance.yahoo.com/quote/INR%3DX?p=INR%3DX", "USD - INR hist data"),
                    tags$br(),
                    tags$a(href = "https://finance.yahoo.com/quote/CNY%3DX?p=CNY%3DX", "USD - CNY hist data")
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                         DT::dataTableOutput("table",width = 300)
                  ))
                  #  tableOutput("table")
                  )
                  
                ),
        # First tab content
        tabItem(tabName = "About",
                fluidPage(
                  titlePanel("About"),
                  mainPanel(
                  h5("I am Terence Lau"),
                  tags$a(href = "https://github.com/TerenceLiu98", "My GitHub"),
                  tags$br(),
                  tags$a(href = "https://terenceliu98.github.io", "My Personal Website"),
                  tags$br(),
                  tags$br(),
                  strong("Thanks to these R package:"), code("shiny, shinydashboard, DT, quantmod")
                  )
                )
                )
    )
  )
)
server <- function(input, output, session) {

  output$plot1 <- renderPlot({
    inFile <- input$inputdata
    inFile <- as.data.frame(inFile)
    if (is.null(inFile))
      return(NULL)
    
    inFile <- read.csv(inFile$datapath, header = input$header)
    # inFile$Date <- as.Date(as.character(inFile$Date), format = '%B %d, %Y')
    model <- loess(Price~as.numeric(as.Date(inFile$Date)), data = inFile, control=loess.control(surface="direct"))
    par(mfrow = c(1, 1))
    plot(x = inFile$Date, y = inFile$Price, type = "b", col = "blue", xlab = "date", ylab = "Price")
    lines(inFile$Date, model$fitted, col="red", type = "l")
    # plot(x = inFile$Date[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], y = inFile$Change..[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], type = "b", col = "blue", xlab = "Dates", ylab = "Changes")
    # FT_Kplot(inFile$Open[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], inFile$High[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], inFile$Low[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], inFile$Price[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], inFile$Date[which(inFile$Date == input$date1):which(inFile$Date == input$date2)])
    })

  
  # sorted columns are colored now because CSS are attached to them
  output$result1 <- renderText({
    inFile <- input$inputdata
    inFile <- as.data.frame(inFile)
    if (is.null(inFile))
      return(NULL)
    
    inFile <- read.csv(inFile$datapath, header = input$header)
    # inFile$Date <- as.Date(as.character(inFile$Date), format = '%B %d, %Y')
    model <- loess(Price~as.numeric(as.Date(inFile$Date)), data = inFile, control=loess.control(surface="direct"))
    predict(model, as.numeric(as.Date(Sys.Date())))
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "USD_INR" = INR,
           "USD_CNY" = CNY)
  })
  
  # Table of selected dataset ----
  output$table <- DT::renderDataTable({
    # datasetInput()
    DT::datatable(data = datasetInput(), options = list(orderClasses = TRUE, scrollX = TRUE, engthMenu = c(5, 30, 50), pageLength = 5))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # output$result1 <- rednerPrint({
  #   inFile <- input$INR
  #   inFile <- as.data.frame(inFile)
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   inFile <- read.csv(inFile$datapath, header = input$header)
  #   inFile$Date <- as.Date(as.character(inFile$Date), format = '%B %d, %Y')
  #   lm_model <- glm(Price[which(inFile$Date == input$date1):which(inFile$Date == input$date2)]~Date[which(inFile$Date == input$date1):which(inFile$Date == input$date2)], data = inFile)
  #   Value <- c(input$Value1, input$Value2, input$Value3)
  #   
  #   })
 #  # INR HOMO
 # output$result1 <- renderPrint({
 #   inFile <- input$INR
 #   inFile <- as.data.frame(inFile)
 #   if (is.null(inFile))
 #     return(NULL)
 #   
 #   inFile <- read.csv(inFile$datapath, header = input$header)
 #   inFile$Date <- as.Date(as.character(inFile$Date), format = '%B %d, %Y')
 #   lm_model <- lm(inFile$Price[which(inFile$Date == input$date1):which(inFile$Date == input$date2)]~., data = inFile)
 #   value1 <- as.numeric(input$inr_homo_dayOne)
 #   value2 <- as.numeric(input$inr_homo_dayTwo)
 #   value3 <- as.numeric(input$inr_homo_dayThree)
 #   result1 <- (value1 + value2 + value3)
 #   cat(result1)
 #     }) 
}
shinyApp(ui, server)