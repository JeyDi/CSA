#Utility file
#This file implement some usefull functions for loading all the environment and datasets
#Call this file by doing: source("utility.r") in your file


environmentSettings <- function(){
  #clear the console
  cat("\014")
  cat("Utility Started\n")
  #clear all old variable from workspace:
  #Uncomment this if you want to restart from empty situation
  rm(list = ls())
  
  #Update the R Version
  # installing/loading the package:
  if(!require(installr)) {
    install.packages("installr"); 
    require(installr);
    library(installr);
    # using the package:
    updateR() # this will start the updating process of your R installation.  It will check for newer versions, and if one is available, will guide you through the decisions you'd need to make.
  } #load / install+load installr
  
  #Install tidyverse if required and not installed
  #ATTENTION: if the installation failed 
  if(!require(tidyverse) ||
     !require(DT) ||
     !require(DescTools) ||
     !require(qtlcharts) ||
     !require(ggvis) ||
     !require(shiny) ||
     !require(radarchart) ||
     !require(mclust) ||
     !require(plotly) ||
     !require(fpc) ||
     !require(seriation) ||
     !require(factoextra)
  ){
    #need manually set the repo without https for certain installations
    options(repos='http://cran.rstudio.com/')
    install.packages(c("tidyverse",
                       "DT",
                       "DescTools",
                       "qtlcharts",
                       "ggvis",
                       "shiny",
                       "radarchart",
                       "mclust",
                       "plotly",
                       "fpc",
                       "seriation",
                       "viridisLite",
                       "viridis",
                       "factoextra"));
    
    library(tidyverse)
    library(psych)
    library(DT)
    library(DescTools)
    library(qtlcharts)
    library(ggvis)
    library(shiny)
    library(radarchart)
    require(mclust)
    library(plotly)
    library(cluster)
    library(fpc)
    library(seriation)
    library(factoextra)
    
  } else{
    
    #Just load the packages
    library(tidyverse)
    library(psych)
    library(DT)
    library(DescTools)
    library(qtlcharts)
    library(ggvis)
    library(shiny)
    library(radarchart)
    require(mclust)
    library(plotly)
    library(mclust)
    library(cluster)
    library(fpc)
    library(seriation)
    library(factoextra)
  }
  
  # #ask to the user the input path for his WD
  # readPath <- function()
  # { 
  #   path <- readline(prompt="Insert your path (carefull with backslash): ")
  #   path <- as.character(gsub("\\\\", "/", path))
  #   print(paste("your input path: ", path))
  #   return(path)
  # }
  # path <- readPath()
  # setwd(path)
  
}


loadDataset <- function(pathName, fileLightName){
  
  path = pathName
  #fileNormal = "FinalDataset.csv"
  fileLight = fileLightName
  #projectFileNormal = paste(path, fileNormal, sep = "/")
  projectFileLight =  paste(path, fileLight, sep = "/")
  
  #Import the datasets if not exists in the environment
  # if(!exists("FinalDataset")){
  #   FinalDataset <- read.csv(projectFileNormal, header=TRUE, sep=";", stringsAsFactors = FALSE, na.strings= "NULL")
  # }
  
  if(!exists("FinalDatasetLight")){
    FinalDatasetLight <- read.csv(projectFileLight, header=TRUE, sep=";", stringsAsFactors = FALSE, na.strings= "NULL")
  }
  cat("Utility completed\n")
  return(FinalDatasetLight)
}

trasformDataset <- function(inputDataset){
  
  cat("\nConverting the dataset..\n")
  
  inputDataset <<- inputDataset
  # names(inputDataset)[1]<-paste("id" )
  
  #Try to convert the initial dataframe and replacing all missing values
  inputDataset$id <- as.numeric(inputDataset$id)

  inputDataset$height <- as.numeric(gsub(",",".",inputDataset$height))
  
  #delete all the char elements
  FinalDatasetLight <-
    inputDataset %>%
    select(-player_name, -strong_foot, -quote, -birthday, -work_rate_att, -work_rate_def)
  
  NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  FinalDatasetLight[] <- lapply(FinalDatasetLight, NA2mean)
  
  FinalDatasetLight <- na.omit(FinalDatasetLight)
  
  FinalDatasetLight$player_name <- inputDataset$player_name
  FinalDatasetLight$strong_foot <- inputDataset$strong_foot
  FinalDatasetLight$quote <- inputDataset$quote
  FinalDatasetLight$birthday <- inputDataset$birthday
  FinalDatasetLight$work_rate_att <- inputDataset$work_rate_att
  FinalDatasetLight$work_rate_def <- inputDataset$work_rate_def
  
  cat("\nConversation completed\n")
  write.csv2(FinalDatasetLight, file = "FinalDatasetLightEdited.csv",row.names = TRUE)
  return(FinalDatasetLight)
}

datasetDescription <- function(inputDataset){
  
  datasetDescription <- describe(inputDataset)
  write.csv2(datasetDescription, file = "datasetDescription.csv",row.names = TRUE)
  return(datasetDescription)
}

writeDatasetCSV <- function(inputDataset, name){
  
  fileName = paste(name, ".csv")
  write.csv2(inputDataset, file = fileName,row.names = TRUE)
  return(inputDataset)
}

#Not usefull yet
clusterDescription <- function(inputCluster){
  cluster <- as.data.frame(inputCluster$cluster)
  centers <- as.data.frame(inputCluster$centers)
  size <- as.data.frame(inputCluster$size)
  
  write.csv2(cluster, file = "clusterResults.csv",row.names = TRUE)
  write.csv2(centers, file = "centersResults.csv",row.names = TRUE)
  write.csv2(size, file = "sizeResults.csv",row.names = TRUE)
  return()
}