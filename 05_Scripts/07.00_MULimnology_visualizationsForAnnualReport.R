##Visualizations for Slap final report for Missouri Reservoirs####
##Created 26Feb2024 by David Richardson (hereafter DCR)
##Creates figures and maps from annual data####

#Libraries
if (!require(maps)) {install.packages("maps")}
if (!require(ggmap)) {install.packages("ggmap")}
if (!require(ggspatial)) {install.packages("ggspatial")}
if (!require(ggrepel)) {install.packages("ggrepel")}
if (!require(patchwork)) {install.packages("patchwork")}

library(maps)
library(ggmap)
library(tidyverse)
library(ggspatial) #For the scale bar and compass on the map
library(ggrepel) #For fancy non-overlapping labels
library(patchwork) #for multipanel plots
library(readxl) #read in excel files, need to load readxl explicity because it is not a core tidyverse package

#Read in the functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#establish the year here###
year<-2023
#Read in the 2023 database here####
waterChemDF<-read_csv(paste0("06_Outputs/",year,"_MissouriReservoirsForDNR_v1.csv"))

#Data munging the data frame####
waterChemDF_summary<-waterChemDF%>%
  filter(!(MULakeNumber=="401"))%>% #remove 401 (field blanks)
  filter(nchar(MULakeNumber)<=3)%>% #remove all field dupes and other sites
  group_by(MULakeNumber,Date,parameterType,endDepth_char,unit,waterBody)%>%
  summarize(parameterValue=mean(parameterValue,na.rm=TRUE),  
            endDepth_m=mean(endDepth_m,na.rm=TRUE),
            samplingSiteLatitude=mean(samplingSiteLatitude,na.rm=TRUE),
            samplingSiteLongitude=mean(samplingSiteLongitude,na.rm=TRUE)
            ) 

#Create averages of each variable for each MULakeNumber across the year####
waterChemDF_trophic<-waterChemDF_summary%>%
    ungroup()%>%
    group_by(MULakeNumber,endDepth_char,parameterType,unit)%>%
    summarize(parameterValue=mean(parameterValue,na.rm=TRUE),  
            endDepth_m=mean(endDepth_m,na.rm=TRUE),
            samplingSiteLatitude=mean(samplingSiteLatitude,na.rm=TRUE),
            samplingSiteLongitude=mean(samplingSiteLongitude,na.rm=TRUE)
            )%>%
    mutate(trophicState=case_when(
                        parameterType=="TP"&parameterValue<10~"Oligotrophic",
                        parameterType=="TP"&parameterValue>=10&parameterValue<25~"Mesotrophic",
                        parameterType=="TP"&parameterValue>=25&parameterValue<100~"Eutrophic",
                        parameterType=="TP"&parameterValue>=100~"Hypereutrophic",
                        parameterType=="TN"&parameterValue<350~"Oligotrophic",
                        parameterType=="TN"&parameterValue>=350&parameterValue<550~"Mesotrophic",
                        parameterType=="TN"&parameterValue>=550&parameterValue<1200~"Eutrophic",
                        parameterType=="TN"&parameterValue>=1200~"Hypereutrophic",
                        parameterType=="CHL_A_COR"&parameterValue<3.0~"Oligotrophic",
                        parameterType=="CHL_A_COR"&parameterValue>=3.0&parameterValue<9.0~"Mesotrophic",
                        parameterType=="CHL_A_COR"&parameterValue>=9.0&parameterValue<40.0~"Eutrophic",
                        parameterType=="CHL_A_COR"&parameterValue>=40.0~"Hypereutrophic",
                        parameterType=="CHL_A_lab"&parameterValue<3.0~"Oligotrophic",
                        parameterType=="CHL_A_lab"&parameterValue>=3.0&parameterValue<9.0~"Mesotrophic",
                        parameterType=="CHL_A_lab"&parameterValue>=9.0&parameterValue<40.0~"Eutrophic",
                        parameterType=="CHL_A_lab"&parameterValue>=40.0~"Hypereutrophic",
                        parameterType=="SECCHI"&parameterValue>=2.6~"Oligotrophic",
                        parameterType=="SECCHI"&parameterValue<2.6&parameterValue>=1.3~"Mesotrophic",
                        parameterType=="SECCHI"&parameterValue<1.3&parameterValue>=0.45~"Eutrophic",
                        parameterType=="SECCHI"&parameterValue<0.45~"Hypereutrophic",
                        .default=NA
    ))%>%
  mutate(trophicState_factor=factor(trophicState,levels=c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic")))

#Graph the different types of chlorophyll####
ggplot(data=waterChemDF_trophic%>%filter(endDepth_char=="EPI")%>%dplyr::select(MULakeNumber,parameterType,parameterValue)%>%filter(parameterType=="CHL_A_COR"|parameterType=="CHL_A_lab")%>%pivot_wider(.,names_from=parameterType,values_from=parameterValue),aes(x=CHL_A_COR,y=CHL_A_lab))+geom_point()+geom_abline(slope=1,intercept=0)+theme_bw()

#Read in state data
state<-map_data('state')  
head(state)

#Read in county data
county<-map_data('county')

#REPORT FIGURE 1: Graph maps of sites with MULakeNumber as the site ID####
#Set the x and y limits
lat_lim <- c(36, 40.5)
lon_lim <- c(-96, -89)

# Let coord_quickmap figure out the correct aspect ratio for export:####
#https://community.rstudio.com/t/aspect-ratio-of-a-plot-produced-with-ggplot2-coord-quickmap/9282/2
coord <- coord_quickmap(xlim = lon_lim, ylim = lat_lim, expand = F)
asp <- coord$aspect(list(x.range = lon_lim, y.range = lat_lim))
asp
#> [1] 0.8185998

#Desired plot width in inches
plot.width<-6
# Calculate height
plot.height.new <- plot.width * asp

gg.MapMULakeNUmber<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey")+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white")+
  geom_point(data=waterChemDF_trophic,aes(x=samplingSiteLongitude,y=samplingSiteLatitude),shape=21,size=3,fill="dark grey")+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "tr")+
  geom_label_repel(data=waterChemDF_trophic%>%filter(parameterType=="TP"&endDepth_char=="EPI"),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,label=MULakeNumber),fill=alpha("white",0.9),size=2,min.segment.length = 0)+
  #coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Export the plot as a jpg####
ggsave(gg.MapMULakeNUmber,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure1-",year,".jpg"),width=plot.width,height=plot.height.new,units="in",dpi=300)

#Set some parameters globally for all maps####
endDepth<-"SURF" #can be EPI or SURF here
pointSize<-2.25 #size of points on the graph
GeoLineWidth<-0.2 #sets the line width of the state and counties on the map

#Map TP with fill=trophicState based on EPI####
gg.MapTP<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey",linewidth=GeoLineWidth)+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white",linewidth=GeoLineWidth)+
  geom_point(data=waterChemDF_trophic%>%filter(parameterType=="TP"&endDepth_char==endDepth),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,fill=trophicState_factor),shape=21,size=pointSize)+
  scale_fill_manual(values = c("white", rgb(180,198,239,maxColorValue = 255), rgb(71,123,200,maxColorValue = 255),rgb(45,69,123,maxColorValue = 255)))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "bl",pad_x=unit(0.1,"in"),pad_y=unit(0.3,"in"))+
  #coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Total phosphorus")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position=c(0.87,0.83))

#Export the plot as a jpg####
ggsave(gg.MapTP,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure2-TP-",year,".jpg"),width=plot.width,height=plot.height.new,units="in")


#Map TN with fill=trophicState based on EPI####
gg.MapTN<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey",linewidth=GeoLineWidth)+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white",linewidth=GeoLineWidth)+
  geom_point(data=waterChemDF_trophic%>%filter(parameterType=="TN"&endDepth_char==endDepth),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,fill=trophicState_factor),shape=21,size=pointSize)+
  scale_fill_manual(values = c("white", rgb(193,226,179,maxColorValue = 255), rgb(81,131,52,maxColorValue = 255),rgb(55,90,34,maxColorValue = 255)))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "bl",pad_x=unit(0.1,"in"),pad_y=unit(0.3,"in"))+
  #coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Total nitrogen")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position=c(0.87,0.83))

#Export the plot as a jpg####
ggsave(gg.MapTN,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure2-TN-",year,".jpg"),width=plot.width,height=plot.height.new,units="in")

#Map corrected chlorophyll-a with fill=trophicState based on EPI####
gg.MapChlAcorr<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey",linewidth=GeoLineWidth)+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white",linewidth=GeoLineWidth)+
  geom_point(data=waterChemDF_trophic%>%filter(parameterType=="CHL_A_COR"&endDepth_char==endDepth),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,fill=trophicState_factor),shape=21,size=pointSize)+
  scale_fill_manual(values = c("white", rgb(255,253,9,maxColorValue = 255), rgb(237,154,15,maxColorValue = 255),rgb(244,3,0,maxColorValue = 255)))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "bl",pad_x=unit(0.1,"in"),pad_y=unit(0.3,"in"))+
  #coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Corrected chl-a")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position=c(0.87,0.83))

#Export the plot as a jpg####
ggsave(gg.MapChlAcorr,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure2-ChlAcorr-",year,".jpg"),width=plot.width,height=plot.height.new,units="in")

#Map secchi with fill=trophicState based on ACTUAL####
gg.MapSecchi<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey",linewidth=GeoLineWidth)+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white",linewidth=GeoLineWidth)+
  geom_point(data=waterChemDF_trophic%>%filter(parameterType=="SECCHI"),aes(x=samplingSiteLongitude,y=samplingSiteLatitude,fill=trophicState_factor),shape=21,size=pointSize)+
  scale_fill_manual(values = c("white", rgb(218,189,241,maxColorValue = 255), rgb(168,101,216,maxColorValue = 255),rgb(114,46,171,maxColorValue = 255)))+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "bl",pad_x=unit(0.1,"in"),pad_y=unit(0.3,"in"))+
  #coord_equal()+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="Secchi depth")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position=c(0.87,0.83))

#Export the plot as a jpg####
ggsave(gg.MapSecchi,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure2-Secchi-",year,".jpg"),width=plot.width,height=plot.height.new,units="in")

###################################################STOPPED HERE###############################
#Merge together in 1 four panel frame with A, B, C, D labels in upper left and no x/y labels where appropriate
MapFigureList<-list(gg.MapTN+
                            xlab("")+
                            theme(axis.text.x=element_blank(),
                                  #axis.ticks.x=element_blank(),
                                  legend.text=element_text(size=7),
                                  legend.title=element_text(size=8),
                                  legend.background = element_rect(fill = 'transparent',color=NA), #Remove legend background
                                  legend.key = element_rect(colour = "transparent", fill = 'transparent'), #Remove legend fill around the circles
                                  legend.spacing.y = unit(5, 'pt'), #Squish the legend closer together
                                  legend.key.height=unit(11,"pt"),
                                  legend.key.width=unit(5,"pt")
                                )+
                            annotate("text",x=-96,y=40.5,label="A",hjust=0.8,vjust=0.1),
                          gg.MapTP+
                            xlab("")+
                            ylab("")+
                            theme(axis.text.x=element_blank(),
                                  axis.text.y=element_blank(),
                                  #axis.ticks.x=element_blank(),
                                  legend.text=element_text(size=7),
                                  legend.title=element_text(size=8),
                                  legend.background = element_rect(fill = 'transparent',color=NA), #Remove legend background
                                  legend.key = element_rect(colour = "transparent", fill = 'transparent'), #Remove legend fill around the circles
                                  legend.spacing.y = unit(5, 'pt'), #Squish the legend closer together
                                  legend.key.height=unit(11,"pt"),
                                  legend.key.width=unit(5,"pt")
                            )+
                            annotate("text",x=-96,y=40.5,label="B",hjust=0.8,vjust=0.1),
                          gg.MapChlAcorr+
                            #xlab("")+
                            #ylab("")+
                            theme( #axis.text.x=element_blank(),
                                  #axis.text.y=element_blank(),
                                  #axis.ticks.x=element_blank(),
                                  legend.text=element_text(size=7),
                                  legend.title=element_text(size=8),
                                  legend.background = element_rect(fill = 'transparent',color=NA), #Remove legend background
                                  legend.key = element_rect(colour = "transparent", fill = 'transparent'), #Remove legend fill around the circles
                                  legend.spacing.y = unit(5, 'pt'), #Squish the legend closer together
                                  legend.key.height=unit(11,"pt"),
                                  legend.key.width=unit(5,"pt")
                            )+
                            annotate("text",x=-96,y=40.5,label="C",hjust=0.8,vjust=0.1),
                          gg.MapSecchi+
                            #xlab("")+
                            ylab("")+
                            theme(#axis.text.x=element_blank(),
                              axis.text.y=element_blank(),
                              #axis.ticks.x=element_blank(),
                              legend.text=element_text(size=7),
                              legend.title=element_text(size=8),
                              legend.background = element_rect(fill = 'transparent',color=NA), #Remove legend background
                              legend.key = element_rect(colour = "transparent", fill = 'transparent'), #Remove legend fill around the circles
                              legend.spacing.y = unit(5, 'pt'), #Squish the legend closer together
                              legend.key.height=unit(11,"pt"),
                              legend.key.width=unit(5,"pt")
                            )+
                            annotate("text",x=-96,y=40.5,label="D",hjust=0.8,vjust=0.1)
                )


#Put the plots on a 2x2 matrix####
(gg.fig2<-wrap_plots(MapFigureList,ncol=2,nrow=2)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Export the plot as a jpg####
ggsave(gg.fig2,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure2-",year,".jpg"),width=plot.width*1.3,height=plot.height.new*1.3,units="in",dpi=300)




#Read in historical data from 5 lakes to be highlighted in the report####
# Jamesport Community #228, King #163, Macon #49, Gopher #169, and Winnebago #120#
FiveHistoricalLakes_surf<-read_excel(paste0("08_ParameterMasterSheets/",year,"_ParameterMasterSheets/SLAP 5 lakes -exported 23 Feb 2024.xlsx"),sheet="DNR Output Data")%>%
                     mutate(Date=ymd(beginDateTime),
                            endDepth_char="SURF",
                            MULakeNumber=as.character(MULakeNumber))%>%
                    filter(beginDepth==0&endDepth==0)%>% #Only pull the surface samples
                    rename(endDepth_m=endDepth
                           )%>%
                    dplyr::select(MULakeNumber,waterBody,Date,parameterType,endDepth_char,unit,parameterValue,endDepth_m,samplingSiteLatitude,samplingSiteLongitude)%>%
                    #Convert units to match the data from this year####
                    mutate(parameterValue=case_when(
                                              parameterType=="CHL_A_COR"~parameterValue*1000,
                                              parameterType=="CHL_A_lab"~parameterValue*1000,
                                              parameterType=="NH4"~parameterValue*1000,
                                              parameterType=="NO3"~parameterValue*1000,
                                              parameterType=="PHEO"~parameterValue*1000,
                                              parameterType=="TDN"~parameterValue*1000,
                                              parameterType=="TN"~parameterValue*1000,
                                              parameterType=="TP"~parameterValue*1000,
                                              .default = parameterValue
                                          ))%>%
                    #Change the unit labels####
                    mutate(unit=case_when(
                      parameterType=="CHL_A_COR"~"ug/L",
                      parameterType=="CHL_A_lab"~"ug/L",
                      parameterType=="NH4"~"ug/L",
                      parameterType=="NO3"~"ug/L",
                      parameterType=="PHEO"~"ug PHEO/L",
                      parameterType=="TDN"~"ug/L",
                      parameterType=="TN"~"ug/L",
                      parameterType=="TP"~"ug/L",
                      parameterType=="SECCHI"~"m",
                      parameterType=="PH_field"~"unitless",
                      parameterType=="PH_lab"~"unitless",
                      parameterType=="TEMP"~"C",
                      .default = unit
                    ))%>%
                      #Change the parameter types####
                    mutate(parameterType=case_when(
                      parameterType=="PH_field"~"pH",
                      parameterType=="TEMP"~"Temp", #This should probably be fixed in 2023 data
                      .default = parameterType
                    ))

#Merge the historical data with the 2023 data####
FiveHistoricalLakes_surf_merged<-bind_rows(FiveHistoricalLakes_surf,waterChemDF_summary%>%filter(MULakeNumber%in%unique(FiveHistoricalLakes_surf$MULakeNumber)))

#Check the units for each to make sure they are synced####
FiveHistoricalLakes_surf%>%dplyr::select(parameterType,unit)%>%distinct(.)%>%arrange(parameterType)%>%print(n=Inf)
waterChemDF_summary%>%ungroup()%>%dplyr::select(parameterType,unit)%>%distinct(.)%>%arrange(parameterType)%>%print(n=Inf)



#Find the list of historical lakes to generate figures for####
unique(FiveHistoricalLakes_surf$MULakeNumber)

#Plot all the different variables over time for a specific lake
ggplot(data=FiveHistoricalLakes_surf_merged%>%filter(MULakeNumber=="49"),aes(x=Date,y=parameterValue))+geom_point()+facet_wrap(~parameterType,scales="free_y")


#Take the annual average and SE of that average####
FiveHistoricalLakes_merged_annual<-
    FiveHistoricalLakes_surf_merged%>%
    ungroup()%>%
    mutate(year=year(Date))%>%
    group_by(year,MULakeNumber,waterBody,parameterType,unit)%>%
    dplyr::summarise(parameterValue_mean=mean(parameterValue,na.rm=TRUE),
              parameterValue_sderr=sderr(parameterValue),
              .groups="keep")

#Plot all the different variables over time for a specific lake
ggplot(data=FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber=="49"),aes(x=year,y=parameterValue_mean))+geom_point()+geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+facet_wrap(~parameterType,scales="free_y")

#Find the sen slopes for these variables: CHL_A_COR, PIM, POM, SECCHI, TN, TP, TSS
#group by MULakeNumber, parameterType, unit
FiveHistoricalLakes_merged_annual%>%
  filter(parameterType%in%c("CHL_A_COR", "PIM", "POM", "SECCHI", "TN", "TP", "TSS"))%>% #pull out these 7 variables of interest
  ungroup()%>%
  group_by(MULakeNumber,parameterType,unit)%>%
  summarize(sensSlope_slope=MTCC.sensSlope(year,parameterValue_mean)$coefficients["Year"],
            sensSlope_intercept=MTCC.sensSlope(year,parameterValue_mean)$coefficients["Intercept"],
            sensSlope_pval=MTCC.sensSlope(year,parameterValue_mean)$pval)%>%
  mutate(significance=ifelse(sensSlope_pval<0.05,"*",""))%>%print(n=Inf)


#for each lake - do this set of 4 figures: TP, TN, CHl, Secchi
# Jamesport Community #228, King #163, Macon #49, Gopher #169, and Winnebago #120#
#can do it on a loop or just run each one here
lake.id<-"169" #specify the lake

#TN specific graphs####
labels_TN<-tibble(x=rep(-Inf,4),y.line=c(350,550,1200,NA),y.label=c((350/2),450,(550+1200)/2,1250),y.label.text=c("0","M","E","HE"))
y.max_TN<-max(1255,max(FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="TN")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.TN.lake<-ggplot(data=FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="TN"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_TN$y.line,linetype=2,color="grey")+
    geom_text(data=labels_TN,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(TN~(mu*g*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_TN)))

#TP specific graphs####
labels_TP<-tibble(x=rep(-Inf,4),y.line=c(10,25,100,NA),y.label=c((10/2),(10+25)/2,(25+100)/2,120),y.label.text=c("0","M","E","HE"))
y.max_TP<-max(125,max(FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="TP")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.TP.lake<-ggplot(data=FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="TP"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_TP$y.line,linetype=2,color="grey")+
    geom_text(data=labels_TP,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(TP~(mu*g*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_TP)))

#CHL_A_COR specific graphs####
labels_CHL_A_COR<-tibble(x=rep(-Inf,4),y.line=c(3,9,40,NA),y.label=c((3/2),(3+9)/2,(9+40)/2,43),y.label.text=c("0","M","E","HE"))
y.max_CHL_A_COR<-max(45,max(FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="CHL_A_COR")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.CHL_A_COR.lake<-ggplot(data=FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="CHL_A_COR"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_CHL_A_COR$y.line,linetype=2,color="grey")+
    geom_text(data=labels_CHL_A_COR,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    theme_bw()+

    xlab("Year")+
    ylab(bquote(Chl~italic(a)~(mu*g*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_CHL_A_COR)))

#SECCHI specific graphs####
labels_SECCHI<-tibble(x=rep(-Inf,4),y.line=c(0.45,1.3,2.6,NA),y.label=c((0.45/2),(0.45+1.3)/2,(1.3+2.6)/2,3),y.label.text=c("HE","E","M","O"))
y.max_SECCHI<-max(3.2,max(FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="SECCHI")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.SECCHI.lake<-ggplot(data=FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id&parameterType=="SECCHI"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_SECCHI$y.line,linetype=2,color="grey")+ #Lines for indicating trophic state
    geom_text(data=labels_SECCHI,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+ #labels for trophic state
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(Secchi~(m)))+
    #scale_y_continuous()+
    scale_y_reverse(limits=c(y.max_SECCHI,-0.2))
)

#Find the absolute max and min for the years####
year.range<-FiveHistoricalLakes_merged_annual%>%filter(MULakeNumber==lake.id)%>%ungroup()%>%summarize(min.year=min(year,na.rm=TRUE)-1,max.year=max(year,na.rm=TRUE)+1)%>%slice(1)%>%as.numeric()

###################################################STOPPED HERE###############################
#Merge together in 1 four panel frame with A, B, C, D labels in upper left and no x/y labels where appropriate
IndLakeFigureList<-list(gg.TN.lake+
                      xlab("")+
                      theme(axis.text.x=element_blank(),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                      )+
                      scale_x_continuous(limits=year.range)+
                      annotate("text",x=Inf,y=Inf,label="A",hjust=1.2,vjust=1.2),
                    gg.TP.lake+
                      xlab("")+
                      #ylab("")+
                      theme(axis.text.x=element_blank(),
                            #axis.text.y=element_blank(),
                            #axis.ticks.x=element_blank(),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                            )+
                      scale_x_continuous(limits=year.range)+
                      annotate("text",x=Inf,y=Inf,label="B",hjust=1.2,vjust=1.2),
                    gg.CHL_A_COR.lake+
                      #xlab("")+
                      #ylab("")+
                      theme( #axis.text.x=element_blank(),
                        #axis.text.y=element_blank(),
                        #axis.ticks.x=element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                      scale_x_continuous(limits=year.range)+
                      annotate("text",x=Inf,y=Inf,label="C",hjust=1.2,vjust=1.2),
                    gg.SECCHI.lake+
                      #xlab("")+
                      #ylab("")+
                      theme(#axis.text.x=element_blank(),
                        #axis.text.y=element_blank(),
                        #axis.ticks.x=element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                        )+
                      scale_x_continuous(limits=year.range)+
                      annotate("text",x=Inf,y=-Inf,label="D",hjust=1.2,vjust=1.2)
)


#Put the plots on a 2x2 matrix####
(gg.fig3.lake<-wrap_plots(IndLakeFigureList,ncol=2,nrow=2)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Export the plot as a jpg####
ggsave(gg.fig3.lake,file=paste0("09_Figures/",year,"_DNRreport/","SLAPReport-Figure3-",year,"-MULakeNumber-",lake.id,".jpg"),width=plot.width*1.3,height=plot.height.new*1.3,units="in",dpi=300)


