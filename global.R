GLOBAL_VAR = "loaded from global.R"


annPullData <- function(file , index){
  withProgress( message = "loading" , value = 1,{
  dataList <- lapply(file$datapath, function(x){
    expData <- read_excel(path = x, sheet = "Annuities in Payment", range = index) # Read in Excel tables as a list of data frames
    compyearData <- read_excel(path = x, sheet = "Instructions",range = "B1:B2",col_names = FALSE)
    c(expData,
      "Company" = compyearData[[1]][1],
      "Year" = compyearData[[1]][2]
      )
                                              }
  
                    )
  })
  bind_rows(dataList) %>% #merges lists into dataframe
    pivot_longer(c('Male','Female','Agg / Unknown'),names_to = "Sex", values_to = "NumExposure")  #Converts table to tidy data.

}



lifePullData<-function(file,sheetNames, index){
  
  dataList <- lapply(file$datapath, function(x){
    
    mainData <- read_excel (path = x, sheet = sheetNames, range = index)
    category <- read_excel (path = x, sheet = sheetNames, range = "B3:B5",col_names = FALSE)
    CompYear <- read_excel (path = x, sheet ="Instructions", range = "B1:B2",col_names = FALSE)
    
    c(mainData, 
      "Company" = CompYear[[1]][1],
      "Year" = CompYear[[1]][2],
      "UnderWritten"  =category[[1]][1],
      "SmokerStatus"  =category[[1]][2],
      "Sex"           = category[[1]][3]
    )
 
  }
  
  
  )
 # 

  bind_rows(dataList)%>% #binds list of dataframes into one dataframe, then tidy's it
    pivot_longer(c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","15+"),names_to = "Duration", values_to = "NumExposure") #Converts table to tidy data. 
 
}


sumData <- function(df){ #Sums larger dataset, along different companies 
  
  df[is.na(df)] <- 0 
  df <- df %>%
    group_by(Company,Age,Sex,Year) %>%
    summarise(SumExposure = sum(NumExposure),SumDeaths =sum(NumDeaths))
  
}



?withProgress