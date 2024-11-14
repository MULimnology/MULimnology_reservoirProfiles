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



#Get list of lakes sorted by their max depth####
MUlakes<-summary3%>%
  group_by(MULakeNumber)%>%
  dplyr::select(MULakeNumber,maxDepth_m)%>%
  summarize(maxDepth_m=mean(maxDepth_m))%>%
  arrange(maxDepth_m)%>%
  print(n=Inf)


#Export as a page per####
pdf(paste0("06_Outputs/NSFPE_ProfilePlots2024.pdf"), onefile = TRUE,width=8.5,height=5)
#lake.index<-1
#for loop here
for(lake.index in 1:nrow(MUlakes)){
MULakeNumber.i<-MUlakes$MULakeNumber[lake.index]
#Filter out level3 and level2 data for that MULakeNumber
level3_sub<-summary3%>%filter(MULakeNumber==MULakeNumber.i)
#Generate an extra variable that is the depth as proportion of the max####
level2_sub<-level2%>%
            filter(MULakeNumber==MULakeNumber.i)%>%
            left_join(.,level3_sub,by="date")%>%
            mutate(depth_proportionOfMax_m=depth_m*100/maxDepth_m)%>%
            group_by(date)
#Generate a figure with profiles for each date####
gg.perlake<-ggplot(level2_sub,aes(y=depth_proportionOfMax_m,x=temp_degC/3))+
  geom_path(color="grey",lwd=1.2)+ #temperature greyed in the background
  geom_path(aes(x=chlorophyll_RFU),color="seagreen3",lwd=2)+ #geom_path to connect in order that data appear in data frame
  geom_path(aes(x=phycocyaninBGA_RFU),color="turquoise",lwd=2)+
  geom_path(aes(x=as.numeric(phycoerythrinTAL_RFU)),color="rosybrown2",lwd=1.2)+
  scale_y_reverse(limits=c(100,0))+
  #scale_y_continuous(limits=c(0,50))+
  labs(x=bquote(Photopigment~(RFU)),y="Depth (percent of max)",title=paste0("MULakeNumber=",MULakeNumber.i," MaxDepth_m=",MUlakes$maxDepth_m[lake.index]))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~date,ncol=4)
print(gg.perlake) #print out the figure
} #end of for loop
dev.off()


#List of dates and MULakeNumbers for a depth vs. RFU figure####
selected_profiles<-tibble(date=as.Date(c("2024-07-24","2024-08-07","2024-07-26","2024-07-17","2024-07-10","2024-07-16")),
       MULakeNumber=c("098","093","039","087","057","183")
         )

#Get out the summary3 data for the selected profiles####
summary3_selected<-left_join(selected_profiles,summary3,by=c("date","MULakeNumber"))
#Get out the level2 data for the selected profiles
level2_selected<-left_join(selected_profiles,level2,by=c("date","MULakeNumber"))
#Merge the two together and calculated a new column that is the depth as a percent of the total depth####
data_selected<-left_join(level2_selected,summary3_selected)%>%
              mutate(depth_percentOfMax_m=depth_m*100/maxDepth_m)%>%
              mutate(maxDepth_m_factor=factor(paste0(round(maxDepth_m,1),"m"),levels=paste0(round(summary3_selected%>%dplyr::select(maxDepth_m)%>%arrange(maxDepth_m)%>%pull(),1),"m")))

#Graph plots####
gg.PEprofileByDepth<-ggplot(data_selected,aes(y=depth_percentOfMax_m,x=as.numeric(phycoerythrinTAL_RFU)))+
  geom_path(aes(x=temp_degC/5,color=temp_degC),lwd=1)+ #temperature greyed in the background
  scale_color_distiller(type = "seq",
                        direction = 1,
                        palette = "Greys")+
  #geom_hline(yintercept=unique(data_selected$thermoclineDepth_m_thresh0.3))+
  geom_path(aes(x=chlorophyll_RFU,fill="chl"),color="seagreen3",lwd=1.2)+ #geom_path to connect in order that data appear in data frame
  geom_path(aes(x=phycocyaninBGA_RFU,fill="phy"),color="turquoise",lwd=1.2)+
  geom_path(aes(fill="pe"),color="rosybrown2",lwd=1.2)+
  scale_y_reverse(limits=c(100,0))+
  #scale_y_continuous(limits=c(0,50))+
  labs(x=bquote(Photopigment~(RFU)),y="Depth (percent of max)",color=bquote(Temp~(degree*C)))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), #remove x axis tick marks and labels
        axis.ticks.x=element_blank()
        )+
  facet_wrap(~maxDepth_m_factor,ncol=6,scale="free_x")

ggsave(paste0("06_Outputs/NSFPE_PEProfilesSelected_byMaxDepth.jpg"),dpi=600,height=4,width=6,units="in")

#try a different way####
data_selected_longer<-data_selected%>%
  dplyr::select(maxDepth_m_factor,depth_percentOfMax_m,chlorophyll_RFU,phycocyaninBGA_RFU,phycoerythrinTAL_RFU)%>%
  pivot_longer(-c(maxDepth_m_factor,depth_percentOfMax_m),names_to = "names",values_to="values")

#Get the thermocline depth####
thermocline_depth<-data_selected%>%
                    dplyr::select(maxDepth_m_factor,thermoclineDepth_m_thresh0.3,maxDepth_m)%>%
                    mutate(thermoclineDepth_m_thresh0.3_percentMax=thermoclineDepth_m_thresh0.3*100/maxDepth_m)%>%
                    group_by(maxDepth_m_factor)%>%
                    filter(row_number()==1)

#Get the top_metalimnion_m depth####
top_metalimnion<-data_selected%>%
  dplyr::select(maxDepth_m_factor,top_metalimnion_m,maxDepth_m)%>%
  mutate(top_metalimnion_m_percentMax=top_metalimnion_m*100/maxDepth_m)%>%
  group_by(maxDepth_m_factor)%>%
  filter(row_number()==1)

#Get temp data and scale it to the max RFU for phycoerythrin
scaled_temp_df<-left_join(data_selected%>%
            group_by(maxDepth_m_factor)%>%
            summarize(maxPE=max(phycoerythrinTAL_RFU),
                      maxTemp=max(temp_degC),
                      minTemp=min(temp_degC)
                      ),
          data_selected%>%dplyr::select(maxDepth_m_factor,depth_percentOfMax_m,temp_degC),
          by=c("maxDepth_m_factor")
        )%>%
  mutate(temp_scaled=maxPE*(temp_degC-minTemp)/(maxTemp-minTemp))

#use long format to print each out faceted####
gg.PEprofileByDepth_alt<-ggplot(data_selected_longer%>%filter(names!="temp_degC"),aes(y=depth_percentOfMax_m,x=values,color=names))+
  geom_path(data=scaled_temp_df,aes(x=temp_scaled,y=depth_percentOfMax_m),color="lightgrey",lwd=1.1)+
  geom_path(lwd=1.1)+
  scale_color_manual(values=c("chlorophyll_RFU"="seagreen3","phycocyaninBGA_RFU"="turquoise","phycoerythrinTAL_RFU"="rosybrown2","Temp_degC"="lightgrey"),limits=c("chlorophyll_RFU","phycocyaninBGA_RFU","phycoerythrinTAL_RFU","Temp_degC"),labels=c("Chlorophyll","Phycocyanin","Phycoerythrin","Temperature"))+
  facet_wrap(~maxDepth_m_factor,ncol=6,scale="free_x")+
  labs(x=bquote(Photopigment~(RFU)),y="Depth (percent of max)",color="")+
  #geom_hline(data=thermocline_depth,aes(yintercept=thermoclineDepth_m_thresh0.3_percentMax))+ #graph the thermocline
  #geom_hline(data=top_metalimnion,aes(yintercept=top_metalimnion_m_percentMax),color="red")+ #graph the top of meta as horizontal line
  scale_y_reverse(limits=c(100,0))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), #remove x axis tick marks and labels
        axis.ticks.x=element_blank()
  )

ggsave(gg.PEprofileByDepth_alt,file=paste0("06_Outputs/NSFPE_PEProfilesSelected_byMaxDepth_alt.jpg"),dpi=600,height=3.3,width=6,units="in")



#look at peak PE relative to peak PC depth####
peak_ratios_df<-summary3%>%
           mutate(ratio_depth_ChltoPC=depthChlMax_m/depthBGAMax_m,
                  ratio_depth_ChltoPE=depthChlMax_m/depthPEMax_m,
                  ratio_depth_PCtoPE=depthBGAMax_m/depthPEMax_m)%>%
        dplyr::select(MULakeNumber,date,maxDepth_m,depthChlMax_m,depthBGAMax_m,depthPEMax_m,ratio_depth_ChltoPC,ratio_depth_ChltoPE,ratio_depth_PCtoPE)

ggplot(data=peak_ratios_df,aes(x=maxDepth_m,y=depthChlMax_m))+
  geom_point(color="seagreen3")+
  geom_point(aes(y=depthBGAMax_m),color="turquoise")+
  geom_point(aes(y=depthPEMax_m),color="rosybrown2")

#Ratio of Phycocyanin to Phycoerythrin####
ggplot(data=peak_ratios_df,aes(x=maxDepth_m,y=ratio_depth_PCtoPE))+
  geom_hline(yintercept=1,color="black")+
  annotate("text",x=40,y=50,label="PC peak is closer to surface",color="red")+
  annotate("text",x=40,y=1.5,label="PC and PE peaks are the same",color="red")+
  annotate("text",x=40,y=0.02,label="PE peak is closer to surface",color="red")+
  #geom_text(data=null(),aes(x=c(50,50,50),y=c(10,1.2,0.1),label=c("a","b","c")))+
  geom_point()+
  scale_y_log10()+theme_bw()

#Ratio of Chl to Phycoerythrin####
ggplot(data=peak_ratios_df,aes(x=maxDepth_m,y=ratio_depth_ChltoPE))+
  geom_hline(yintercept=1,color="black")+
  annotate("text",x=40,y=50,label="Chl peak is closer to surface",color="red")+
  annotate("text",x=40,y=1.5,label="Chl and PE peaks are the same",color="red")+
  annotate("text",x=40,y=0.02,label="PE peak is closer to surface",color="red")+
  #geom_text(data=null(),aes(x=c(50,50,50),y=c(10,1.2,0.1),label=c("a","b","c")))+
  geom_point()+
  scale_y_log10()+theme_bw()

#Ratio of chl to Phycyanonin####
ggplot(data=peak_ratios_df,aes(x=maxDepth_m,y=ratio_depth_ChltoPC))+
  geom_hline(yintercept=1,color="black")+
  annotate("text",x=40,y=50,label="Chl peak is closer to surface",color="red")+
  annotate("text",x=40,y=1.5,label="Chl and PC peaks are the same",color="red")+
  annotate("text",x=40,y=0.02,label="PC peak is closer to surface",color="red")+
  #geom_text(data=null(),aes(x=c(50,50,50),y=c(10,1.2,0.1),label=c("a","b","c")))+
  geom_point()+
  scale_y_log10()+theme_bw()

ggplot(data=peak_ratios_df,aes(x=depthBGAMax_m,y=depthChlMax_m))+
  geom_point()
