
library(shiny)

ui = fluidPage(
      titlePanel("Mortality Data App"),
      helpText("Welcome Message"),
      
      sidebarLayout(
        
        sidebarPanel(
          
          fileInput("uploadFiles","Insert files here. Be sure to select all files at once.", multiple = TRUE),
          #selectInput(inputID = 'dataoption',label = "Select the section of insurance you'd like to see", choices = c("Annuiants/Pensioners", "Life Insurance")),
          uiOutput("companyfilter"),
          uiOutput("sexfilter"),
          uiOutput("yearFilter"),
          uiOutput("downloadlink")
          
        ),
        
      mainPanel(
        tableOutput('data')
        
      )
    )
    )
