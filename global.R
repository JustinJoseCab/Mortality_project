GLOBAL_VAR = "loaded from global.R"


annPullData <- function(file , index){
  
  dataList <- lapply(file$datapath, function(x){
    expData <- read_excel(path = x, sheet = "Annuities in Payment", range = index) # Read in Excel tables as a list of data frames
    compyearData <- read_excel(path = x, sheet = "Instructions",range = "B1:B2",col_names = FALSE)
    c(expData,
      "Company" = compyearData[[1]][1],
      "Year" = compyearData[[1]][2]
      )
                                              }
  
                    )
    
  bind_rows(dataList) %>% #merges lists into dataframe
    pivot_longer(c('Male','Female','Agg / Unknown'),names_to = "Sex", values_to = "NumExposure")  #Converts table to tidy data.

}

sumData <- function(df){ #Sums larger dataset, along different companies 
  
  df[is.na(df)] <- 0 
  df <- df %>%
    group_by(Company,Age,Sex,Year) %>%
    summarise(SumExposure = sum(NumExposure),SumDeaths =sum(NumDeaths))
  
}



