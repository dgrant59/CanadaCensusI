library(tidyverse)
library(readxl)
library(rgdal)
library(ggspatial)
library(magrittr)
library(fuzzyjoin)
library(tmap)
library(RColorBrewer)
library(svglite)
setwd("PATHHERE")


popdata <- read.csv("98-401-X2021006_English_CSV_data.csv")
popdata %<>% filter(GEO_LEVEL=="Census subdivision") %>% filter(CHARACTERISTIC_ID%in%c(1:5))

data_qual <- popdata[,c(3,6)] %>% group_by(ALT_GEO_CODE) %>% summarise(Data_qual = toString(DATA_QUALITY_FLAG))

popdata <- popdata[,-c(6,7,9,11)] %>% group_by(ALT_GEO_CODE) %>% pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL)

popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"))
popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))

###### MAP DATA
canada <- readOGR(dsn = "./ShapeFiles/Alt3/lcsd000b21a_e.shp", 
                   stringsAsFactors = T)

#Join map data to case data for easy ggplotting
canada@data <- left_join(canada@data,popdata, by=c("DGUID"="DGUID")) 


# dates <- colnames(ontario@data)[10:(length(colnames(ontario@data))-1)]

places <- c(10:13,24,35,46:48,59:62)
pnames <- c("Newfoundland and Labrador\n(Terre-Neuve-et-Labrador)",
           "Prince Edward Island\n(Île-du-Prince-Édouard)",
           "Nova Scotia\n(Nouvelle-Écosse)",
           "New Brunswick\n(Nouveau-Brunswick)",
           "Quebec\n(Québec",
           "Ontario",
           "Manitoba",
           "Saskatchewan",
           "Alberta",
           "British Columbia\n(Colombie-Britannique)",
           "Yukon",
           "Northwest Territories\n(Territoires du Nord-Ouest)",
           "Nunavut")

for(i in 1:length(places)){
  temp <- canada[gsub("(^\\d{2}).*", "\\1", as.integer(as.character(canada@data$CSDUID)))==places[i],]
  ggplot()+
    annotation_spatial(temp,lwd=0.05)+
    layer_spatial(temp,aes(fill=`Population percentage change, 2016 to 2021`,color="Population not \nrecorded in 2016"),lwd=0.05)+
    scale_colour_manual(values="black") +  
    scale_fill_stepsn(breaks=c(-2.6,0,2.6,5.2,7.8,10.4,13),na.value="grey",
                      colours =brewer.pal(9,"RdYlGn"),
                         labels = c(-2.6,0,2.6,"5.2 (Canada)",7.8,10.4,13),
                         limits=c(-5.2,15.6),
                         oob=scales::squish,
                         guide = guide_colourbar(ticks.colour="black",
                                                 barheight = 20,
                                                 frame.colour = "black"))+
    labs(fill="% change in Pop, \n 2016-2021")+
    theme(legend.title.align=0.5,
          axis.line=element_blank(),  #bunch of options to remove "graph" visuals
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.title = element_text(hjust = 0.5,size=20),
          legend.text=element_text(size=10),  #legend text sizes
          legend.title=element_text(size=13))+
    ggtitle(paste("Growth of\n",pnames[i],"\n2016-2021"))+
    guides(colour=guide_legend("", override.aes=list(colour="black")))
  
  ggsave(paste0(sub("\\n.*","",pnames[i]),".pdf"),path="./Maps/")
}

# 10 	Newfoundland and Labrador/Terre-Neuve-et-Labrador
# 11 	Prince Edward Island/Île-du-Prince-Édouard
# 12 	Nova Scotia/Nouvelle-Écosse
# 13 	New Brunswick/Nouveau-Brunswick
# 24 	Quebec/Québec
# 35 	Ontario
# 46 	Manitoba
# 47 	Saskatchewan
# 48 	Alberta
# 59 	British Columbia/Colombie-Britannique
# 60 	Yukon
# 61 	Northwest Territories/Territoires du Nord-Ouest
# 62 	Nunavut

#Add interaction?
#canada@data$CSDUID

#tm_shape(temp)+tm_fill(col="Population percentage change, 2016 to 2021")+tm_borders()

