##Script 05.02: Loading and selecting specific level02 profiles for Thin Ice####
##Created 19Dec2025 by David Richardson (hereafter DCR)
##This loads level 2 data from all the years, then subsets specific profiles for Thin Ice####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(lubridate)) {install.packages("lubridate")}
if (!require(stringr)) {install.packages("stringr")}
if (!require(hms)) {install.packages("hms")}

#Load packages
library(tidyverse)
library(lubridate)
library(stringr)
library(hms) #to get out time neatly

#Run functions script to upload all the user defined functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#*Set the directory path here####
dirPath<-paste0("02_Level2_Data/")

#Identify all the individual .csv files####
Level2_files<-list.files(dirPath,pattern = "*.csv")

#Initialize storage location####
List_Level2<-list()

#Loop through all the level3 files and load them individually####
#debug fileIndex<-1
#This goes from 8 to 10 - 2024 and 2025 and 2026, expand out for next year
for(fileIndex in 8:10){
  List_Level2[[fileIndex]]<-read_csv(file=paste0(dirPath,Level2_files[fileIndex]), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
}

#Bind them all together####
Level2_allData<-do.call(bind_rows, List_Level2)

#List of all MULakeNumbers that are in the Thin Ice project
ThinIce_MULakeNumbers<-c("438","088","219","440","112","446","274")

#Filter out only thin ice lakes then split into a list for each profile####
ThinIceSplit<-Level2_allData%>%
  filter(MULakeNumber%in%ThinIce_MULakeNumbers)%>%
  group_by(MULakeNumber,date)%>%
  group_split()

#Loop through all the profiles####
#ThinIceProfile.index<-1
for(ThinIceProfile.index in 1:length(ThinIceSplit)){
  temp.df<-ThinIceSplit[[ThinIceProfile.index]]%>%
            mutate(`date_yyyy-mm-dd`=date,
                   time_hhmmss=as_hms(dateTime)
            )%>%
            dplyr::select(-dateTime,-date)%>%
            dplyr::select(MULakeNumber,`date_yyyy-mm-dd`,time_hhmmss,everything())
  #Get out the MULakeNumber####
  MULakeNumber<-temp.df$MULakeNumber[1]
  
  #Create a file path
  dir.output<-paste0("06_Outputs/ThinIce/Level2_Profiles_",MULakeNumber)
  
  # Check if the folder exists
  if (!file.exists(paste0(dir.output))) {
    # If the file does not exist, create it
    dir.create(paste0(dir.output))
  }
  
  #Generate the file name
  year<-year(temp.df$`date_yyyy-mm-dd`)[1]
  month<-sprintf("%02d",month(temp.df$`date_yyyy-mm-dd`)[1])
  day<-sprintf("%02d",day(temp.df$`date_yyyy-mm-dd`)[1])
    #*File name####
    file.name<-paste(MULakeNumber,year,month,day,"profile.csv",sep="_")
  #Paste out the profile  
  write_csv(x=temp.df,file=paste(dir.output,file.name,sep="/"))
  
}


