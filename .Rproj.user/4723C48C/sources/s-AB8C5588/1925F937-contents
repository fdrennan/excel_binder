#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
# Define UI for application that draws a histogram

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xlsx"),
                multiple = TRUE
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      downloadLink('downloadData', 'Download'),
      tableOutput('tableOut')
    )
  )
)

server <- function(input, output) {
  contents <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    a = read_xlsx(inFile$datapath[1], col_names = TRUE) %>% apply(2, as.character)
    for(i in 2:length(inFile$datapath)) {
      b = read_xlsx(inFile$datapath[i], col_names = TRUE) %>% apply(2, as.character)
      a = rbind(a, b)
    }
    
    a[is.na(a)] = ""
    
    a
    
  })
  
  output$tableOut <- renderTable({
   contents() %>% head
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(contents(), con)
    }
  )
  
}

shinyApp(ui, server)

