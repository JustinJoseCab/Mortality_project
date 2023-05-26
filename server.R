library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)



server = function(input, output, session) {

  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })

  Annuity = reactive({  #Pulls in Annuity Data & Does some preliminary filtering
    
   AnnNumInForce <- annPullData(file = input$uploadFiles, index = "A5:D76")
   AnnNumDeaths <- annPullData(file = input$uploadFiles, index = "A81:D152")
   AnnNumDeaths <- rename(AnnNumDeaths,"NumDeaths" = NumExposure)
  
   Annuitydf <- bind_cols(AnnNumInForce,AnnNumDeaths[5])
   AnnuityDeathRate <- sumData(Annuitydf) %>%
    ungroup() %>%
     group_by(Company,Age,Sex) %>%
     mutate(TwoYearExposure = SumExposure + lag(SumExposure)) %>%
     mutate(DeathRate = 2*SumDeaths / TwoYearExposure) %>%
     filter(DeathRate>0) %>%
     filter(!is.infinite(DeathRate))%>%
     select(-(SumExposure:TwoYearExposure)) 
   
   return(Annuitydf)
  
   } )
  
      
  Insurance = reactive({
    
  
    
    lifeNames <- excel_sheets(path = input$uploadFiles$datapath[1])
    lifeNames <- str_subset(lifeNames,pattern ="^Life")
    lifeNames <- lifeNames[-1]
    LifeNumInForce<- data.frame()
    LifeNumDeaths<- data.frame()
    start.time <- Sys.time() 
    
    
    lapply(lifeNames,function(x){    
      LifeNumInForce<<- bind_rows(LifeNumInForce,lifePullData(file = input$uploadFiles, sheetNames = x , index = "A10:R121")) #<< lexical scoping
      LifeNumDeaths <<-bind_rows(LifeNumDeaths,lifePullData(file = input$uploadFiles, sheetNames = x, index = "A127:R238"))
    }
    
    
      
    )
    
    
    return(LifeNumInForce)
    
  
  })
  
  output$insdata <- renderTable({
    
    
    
    Insurance()
      

    
  })
  
  
  # Company options
  output$companyfilter <- renderUI({
   data <- Annuity()
    if(is.null(data)){return(NULL)}
    
    checkboxGroupInput(inputId = "company",
                label = "Select Company",
                choiceNames = unique(data$Company),
                choiceValues = unique(data$Company))
              
  })
  
  # Company filter function 
  
  company_df <- reactive({
    data<-Annuity()
    if(is.null(data)){return(NULL)}
    data %>%
      filter(Company %in% input$company)

  })
  
  # Sex options
  output$sexfilter <- renderUI({
    data <- company_df()
    if(is.null(data)){return(NULL)}
    
    checkboxGroupInput(inputId = "sex",
                       label = "Select Sex",
                       choiceNames = unique(data$Sex),
                       choiceValues = unique(data$Sex))
    
  })
  
  
  # Sex filter function
  company_and_sex_df <- reactive({
    data <- company_df()
   
    if(is.null(data)){return(NULL)}
    
    data %>% 
      filter(Sex %in% input$sex)
    
  })

  
  # Year options
  output$yearFilter <- renderUI({
    data <- company_and_sex_df()
    if(is.null(data)){return(NULL)}
    
    checkboxGroupInput(inputId = "year",
                       label = "Select Year(s)",
                       choiceNames = unique(data$Year),
                       choiceValues = unique(data$Year))
    
  })
  
  
  #Actual Year filter
  company_year_sex_df <- reactive({
    data <- company_and_sex_df()
    
    if(is.null(data)){return(NULL)}
    
    data %>% 
      filter(Year %in% input$year)
    
  })
  
  
  
  output$data <- renderTable({
    
    company_year_sex_df()%>%
      group_by(Year)%>%
      summarise(Exposure = sum(NumExposure, na.rm = TRUE),
                Deaths = sum(NumDeaths, na.rm = TRUE),
                Companies = n_distinct(Company)
                )
    
                  }
                          )
  
  
  output$plot <- renderPlot({
    data <- company_year_sex_df()
    data$ageRange <- cut(data$Age, breaks =c(0,18,30,40,50,60,70,80,110))
    data$Sex = as.factor(data$Sex)
    ggplot(data, aes(fill = Sex, y = NumExposure,x=ageRange))+
      geom_bar(position='dodge', stat = "identity")
    
  })
  
   output$wd <- renderText(getwd())
                           
                        

#Code for downloading dynamic dataset  
  
  output$downloadlink <- renderUI({
    
    downloadButton("downloadData","Download Data")
    
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste("filetest.csv")
    },
    content = function(file){
      write.csv(company_year_sex_df(),file,row.names = TRUE)
      
    }
    
  )
  
  output$insDownloadlink <- renderUI({
    
    downloadButton("insDownloadData","Download Insurance Data")
    
    
  })

 output$insDownloadData <- downloadHandler(
   
   filename = function(){
     paste("Insurance Dataset.csv")
   },
   content = function(file){
     write.csv(Insurance(),file,row.names = TRUE)
     
   }
 )
   
   
   
  
  
  
  
  
  
  

  
 # output$files <- renderTable(input$uploadFiles) use this to test file dataframe. 
  
  #sexFilter = reactive({
   # output$content %>% filter(Sex ==input$uploadFiles$Sex)
  #})
    
 # output$test <- renderTable(
  #  sexFilter()
  #)

  
  }