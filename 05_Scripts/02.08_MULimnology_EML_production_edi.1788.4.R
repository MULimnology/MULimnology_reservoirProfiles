##Level2: Publishing level2 profiles with EDI####
###Version of edi.1788.4 
##Created 28Aug2026 by David Richardson (hereafter DCR)
##Includes 2023-2026 profiles
##Reads in files from the specific 04_EDI folder and creates EML file for publication in EDI####
##See here for additional instructions from Cayelan Carey's lab: https://github.com/CareyLabVT/Reservoirs/blob/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2022/MakeEMLChemistry.R

#package id
package.id<-"edi.1788.4"

#Folder for this edi###
edi.folder<-paste0("04_EDI/",package.id,"/")

#Start year####
startYear<-2023

# Steps for setting up EML metadata ####
if(!require(EMLassemblyline)){install.packages("EMLassemblyline")}
if(!require(devtools)){install.packages("devtools")}
if(!require(maps)){install.packages("maps")}
if(!require(tidyverse)){install.packages("tidyverse")}

#Load packages
library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
# Install from GitHub
  #remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Ggplot can work well with maps package
library(maps)
library(tidyverse)


  
  
#Create files and attribute tables for all level 2 data####
  #*Identify all the individual .csv files####
  Level2_files<-list.files("02_Level2_Data/",pattern = "*.csv")
  #Find the positions of the years that are 2023 and later, helpful for versions in the future####
  position2023andLaterYears<-which(as.numeric(substr(Level2_files,1,4))>=startYear)
  #Keep only 2022 and before by eliminating 2023 and 2024####
  Level2_files<-Level2_files[position2023andLaterYears]
  
  #Create list for storage of unique sites#####  
  unique_sites_list<-list()  

  #Set the year - this will be in the for loop####
  #file.index<-5
  for(file.index in 1:length(Level2_files)){
  #*Read in a level 2 file####
  temp_level2<-read_csv(paste("02_Level2_Data/",Level2_files[file.index],sep=""))%>%
                mutate(date=as.character(date), #make sure the date exports correctly
                  dateTime=as.character(dateTime)) #make sure the dateTime exports correctly
  
  #Save the temp_level2####
  unique_sites_list[[file.index]]<-temp_level2%>%dplyr::select(MULakeNumber)%>%distinct()
  
  #*pull out the year####
  Extract_year<-sub("\\_.*", "", Level2_files[file.index])
  #*rename extract year for historical####
  if(Extract_year=="Historical"){Extract_year<-"1989-2016"}
  #Create a new EDI friendly file name####
  fileName<-paste(edi.folder,"MissouriReservoirs_ProfileData_",Extract_year,".csv",sep="")
  #*Paste in EDI file####
  write_csv(x=temp_level2,file=fileName)

  #*Check the dictionary for units####
  #view_unit_dictionary()
  #*Create attribute definitions for all level2 data####
  attributeDefinition=c("Missouri University identifier for that lake or reservoir",
                        "Date of sampling",
                        "Date and time of sampling. All data were collected in the central time zone of the U.S.A., with daylight savings time observed",
                        "Water depth where sensor reading was measured",
                        "Water temperature",
                        "Dissolved oxygen concentration",
                        "Dissolved oxygen saturation",
                        "Total Chlorophyll a measured in RFU",
                        "Blue-Green Algae phycocyanin measured in RFU",
                        "Total Algae phycoerythrin measured in RFU",
                        "Turbidity measured in FNU",
                        "Salinity measured in PSU",
                        "Specific conductivity",
                        "Total dissolved solids",
                        "Oxidation Reduction Potential",
                        "pH - potential of hydrogen",
                        "Latitude measured on the sonde handheld during sampling",
                        "Longitude measured on the sonde handheld during sampling",
                        "Altitude measured on the sonde handheld during sampling",
                        "Air barometer pressure measured on the sonde handheld during sampling"
                        )
  #*Create attribute class for all level2 data####
  attributeclass<-c("character","Date","Date",rep("numeric",17))
  
  #*Create attribute unit for all level2 data####
  attributeUnit<-c("",
                   "",
                   "",
                   "meter",
                   "celsius",
                   "milligramsPerLiter",
                   "percent",
                   "RelativeFluorescenceUnits", #Custom
                   "RelativeFluorescenceUnits", #Custom
                   "RelativeFluorescenceUnits", #Custom
                   "FormazinNephelometricUnits", #Custom
                   "PracticalSalinityUnit", #Custom
                   "microSiemensPerCentimeter", #Custom
                   "milligramsPerLiter",
                   "millivolt",
                   "dimensionless",
                   "degree",
                   "degree",
                   "meter",
                   "millibar"
                   )
  
  #*Create the column for the dateTime formats#### 
  attributeDateTimeFormatString<-c("","YYYY-MM-DD","YYYY-MM-DD hh:mm:ss",rep("",17))
  
  #*Create attribute table for the lat/long metadata####
  Level2_attributeTable<-tibble(attributeName=names(temp_level2),
                                              attributeDefinition=attributeDefinition,
                                              class=attributeclass,
                                              unit=attributeUnit,
                                              dateTimeFormatString=attributeDateTimeFormatString,
                                              missingValueCode=NA,
                                              missingValueCodeExplanation=rep("MissingValue",length(names(temp_level2)))
                                )
  #*write out the attribute table####
  write_tsv(x=Level2_attributeTable,file=paste(edi.folder, "attributes_MissouriReservoirs_ProfileData_",Extract_year,".txt",sep=""))  
  
  #Create a catvars file for each year####
  catvars<-tibble(attributeName=c("class","class","class","missingValueCodeExplanation"),code=c("character","Date","numeric","MissingValue"),definition=rep("",4))
  
  #*write out the catvars attribute table for each year####
  write_tsv(x=Level2_attributeTable,file=paste(edi.folder, "catvars_attributes_MissouriReservoirs_ProfileData_",Extract_year,".txt",sep=""))  
  
  
  
  } #End of loop through the different level 2 files 


#########################################################  
#Create files and attribute tables for site metadata####

#Bind all the MULakeNumbers together
unique_MULakeNumbers<-do.call(bind_rows,unique_sites_list)%>%distinct()%>%arrange(MULakeNumber)

#URL for the site data from the public github repository: https://github.com/MULimnology/MULimnology_Sites####
site_url<-c("https://raw.githubusercontent.com/MULimnology/MULimnology_Sites/refs/heads/main/01_MainData/MULimnology-MULakeNumber-samplingSite-Main.csv")

#Read in the site data from the github repository####
all_site_data<-read_csv(site_url)

#Find all the unique sites that were measured- if there are duplicates, save the only the first row which is the dam site####
site_metadata<-left_join(unique_MULakeNumbers,all_site_data)%>%group_by(MULakeNumber)%>%slice_head(n=1)

#*Check the dictionary for units####
#view_unit_dictionary()
#*write out the csv file####
write_csv(x=site_metadata,file=paste0(edi.folder,"MissouriReservoirs_Metadata_SiteData.csv"))
#*Create attribute table for the lat/long metadata####
metaDataFromDatabase_attributeTable<-tibble(attributeName=names(site_metadata),
                                            attributeDefinition=c("Missouri University identifier for that lake or reservoir","Most commonly used waterbody name","First alternate waterbody name","Second alternate waterbody name","Sampling site name","Latitude in decimal degrees","Longitude in decimal degrees","Notes on locations"),
                                            class=c("character","character","character","character","character","numeric","numeric","character"),
                                            unit=c("","","","","","degree","degree",""),
                                            dateTimeFormatString="",
                                            missingValueCode=NA,
                                            missingValueCodeExplanation=rep("MissingValue",length(names(site_metadata))))
#*write out the attribute table####
write_tsv(x=metaDataFromDatabase_attributeTable,file=paste0(edi.folder,"attributes_MissouriReservoirs_Metadata_SiteData.txt"))


  
#Read in state data
missouri_state<-map_data('state')%>%filter(region=="missouri")  
head(missouri_state)

#Plot state with state boundaries just for fun
#ggplot()+
#  geom_polygon(data=missouri_state,aes(x=long,y=lat,group=group),fill='white',color='black')

#find the bounding box of missouri
min(missouri_state$long) #west
max(missouri_state$long) #east
max(missouri_state$lat) #north
min(missouri_state$lat) #south

#Find bounding box of all the profile samples
min(site_metadata$Longitude_samplingSite) #west
max(site_metadata$Longitude_samplingSite) #east
max(site_metadata$Latitude_samplingSite) #north
min(site_metadata$Latitude_samplingSite) #south

#For the bounding box, get the max extents of either the state of Missouri or the furthest waterbody####
west<-min(min(missouri_state$long),min(site_metadata$Longitude_samplingSite)) #west
east<-max(max(missouri_state$long),max(site_metadata$Longitude_samplingSite)) #east
north<-max(max(missouri_state$lat),max(site_metadata$Latitude_samplingSite)) #north
south<-min(min(missouri_state$lat),min(site_metadata$Latitude_samplingSite)) #south

#Create a dataframe of the geographic coverage
geographic_coverage<-tibble(geographicDescription="midwest USA",
       westBoundingCoordinate=west,
       eastBoundingCoordinate=east,
       northBoundingCoordinate=north,
       southBoundingCoordinate=south)

#Write out geographic coverage txt file
write_tsv(x=geographic_coverage,file=paste0(edi.folder,"geographic_coverage.txt"))

# Generate templates for dataset licensed under CCBY, with 3 tables.
template_core_metadata(path = "04_EDI/",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

#Fill all csv files that start with 'attributes'####
attributeFiles<-str_subset(list.files(edi.folder,pattern = "*.txt"),pattern="^attributes")
dataFiles<-str_subset(list.files(edi.folder,pattern = "*.csv"),pattern="^Missouri")

#compile attribute template files - don't do this if they already exist####
# template_table_attributes(path = "04_EDI/",
#                          data.path = "04_EDI/",
#                          data.table = dataFiles)
#Identify which variables are categorical####              
# template_categorical_variables(path = "04_EDI/",
#                                data.path = "04_EDI/",
#                                write.file = TRUE)

#Specify geographic coverage for all our data files####
#Not needed if geograph_coverage.txt is already existing
#template_geographic_coverage(path = "04_EDI/",
#                             data.path = "04_EDI/",
#                             data.table = dataFiles,
#                             empty = TRUE,
#                             write.file = TRUE)

################################
# Run this function
  #Make sure validation passes - address any issues here and rerun####
  #Notes on issues
    #make sure the keywords are tab delimited, have column headers of keyword and keywordThesaurus - some can come from the LTER controlled vocabulary: https://emily.lternet.edu/vocab/vocab/index.php
make_eml(path = edi.folder,
         dataset.title = "Missouri reservoir profile data including depth, temperature, oxygen, photopigments, conductivity, pH, turbidity, and oxidative-reductive potential starting in 2023", 
         data.path = edi.folder,
         eml.path = edi.folder,
         data.table  = dataFiles,
         data.table.name=sub(".csv$","",dataFiles),
         data.table.description = sub(".csv$","",dataFiles),
         temporal.coverage = c("2023-01-01", paste(max(as.numeric(sub("\\_.*", "", Level2_files)),na.rm=TRUE),"-12-31",sep="")), #gives the end date as the last year of the list 31Dec
         maintenance.description = "complete", 
         user.domain = "EDI",
         user.id = "northr",
         package.id=package.id
         )

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using the North Lab username and password. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.


