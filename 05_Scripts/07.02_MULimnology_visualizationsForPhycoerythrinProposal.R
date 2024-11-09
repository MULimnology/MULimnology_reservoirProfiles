##Visualizations for Phycoerythrin proposal for Missouri Reservoirs####
##Created 08Nov2024 by David Richardson (hereafter DCR)
##Creates some potential figures for the report####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}

library(tidyverse)

#Read in the functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#establish the year here###
year<-2024

#Read in the level 3 data for this year####
summary3<-read_csv(file=paste0("03_Level3_Data/",year,"_Level3.csv"))

#Read in the level 2 data for this year
level2<-read_csv(file=paste0("02_Level2_Data/",year,"_Level2.csv"))

#Read in the 2022 database here cause it has DOC####
#########Have to create it in the water chem repo###############################
#waterChemDF<-read_csv(paste0("06_Outputs/",2022,"_MissouriReservoirsForDNR_v1.csv"))

#Get out June samples####
summary3_june<-summary3%>%filter(month(date)==6)%>%arrange(maxDepth_m)%>%print(n=Inf)
level2_june<-level2%>%filter(month(date)==6)

ggplot(level2_june%>%filter(MULakeNumber=="092"),aes(y=depth_m,x=temp_degC/5))+
  geom_path(color="grey",lwd=1.2)+ #temperature greyed in the background
  geom_path(aes(x=chlorophyll_RFU),color="seagreen3",lwd=2)+ #geom_path to connect in order that data appear in data frame
  geom_path(aes(x=phycocyaninBGA_RFU),color="turquoise",lwd=2)+
  geom_path(aes(x=as.numeric(phycoerythrinTAL_RFU)),color="rosybrown2",lwd=1.2)+
  scale_y_reverse(limits=c(50,0))+
  #scale_y_continuous(limits=c(0,50))+
  labs(x=bquote(Photopigment~(RFU)),y="Depth (m)")+
  theme_bw()
