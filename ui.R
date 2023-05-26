
library(shiny)

ui = fluidPage(
  
  
      titlePanel("Mortality Data App"),
      helpText("Welcome Message"),
      

          
          fileInput("uploadFiles","Insert files here. Be sure to select all files at once.", multiple = TRUE),
          #selectInput(inputID = 'dataoption',label = "Select the section of insurance you'd like to see", choices = c("Annuiants/Pensioners", "Life Insurance")),
          
          
      
        
      mainPanel(
        
        tabsetPanel(
          
        tabPanel("Annuity",  
        #textOutput("wd"),
                    sidebarLayout(
                    sidebarPanel(
                     uiOutput("companyfilter"),
                     uiOutput("sexfilter"),
                     uiOutput("yearFilter"),
                     uiOutput("downloadlink")
                                ),  
        
                    mainPanel(
                      tableOutput('data'),
                    plotOutput('plot')
                            )
                              )
                ),
        tabPanel("Insurance",
                    sidebarLayout(
                    sidebarPanel(
                      uiOutput("insDownloadlink")
                                ),
                    
                  
                    mainPanel(
                     # tableOutput('insdata')
                      textOutput('hi')
                      
                              )
                 
                                  )
                 
                )
        
        
        
        )
      )
    )



