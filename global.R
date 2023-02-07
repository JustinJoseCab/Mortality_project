GLOBAL_VAR = "loaded from global.R"


annPullData <- function(file , index){
  dataList <- lapply(file$datapath, function(x)
    read_excel(path = x, sheet = "Annuities in Payment", range = index)) # Read in Excel tables as a list of data frames
  

  
  for (i in seq_along(dataList)){
    year <- read_excel(path = file$datapath[i],sheet ="Instructions", range = "B2",col_names = FALSE) #Pulls year value from file
    company <- read_excel(path = file$datapath[i],sheet ="Instructions", range = "B1",col_names = FALSE) 
    
    
    dataList[[i]]<- dataList[[i]] %>%
     
     mutate(newcol = company[[1]]) %>%
     rename("Company" = newcol) %>%
      mutate(newcol = year[[1]]) %>%
      rename("Year" = newcol)
    
  }

    
  bind_rows(dataList) %>% #merges lists into dataframe
    
    pivot_longer(c('Male','Female','Agg / Unknown'),names_to = "Sex", values_to = "NumExposure")  #Converts table to tidy data.
    #mutate(Company = str_remove_all(Company,"Mortality Project |.xlsx")) %>% #Cleaning up unique identifier, removes .xlsx.   
    #mutate(Company = str_remove_all(Company,"\\d"))

}

sumData <- function(df){ #Sums larger dataset, along different companies 
  
  df[is.na(df)] <- 0 
  df <- df %>%
    group_by(Company,Age,Sex,Year) %>%
    summarise(SumExposure = sum(NumExposure),SumDeaths =sum(NumDeaths))
  
}



