library(tidyverse)
library(readxl)
library(rgdal)
library(ggspatial)
library(magrittr)
library(fuzzyjoin)
library(tmap)
library(RColorBrewer)
library(svglite)
library(sf)
#setwd("PATHHERE")


popdata <- read.csv("98-401-X2021006_English_CSV_data.csv")
popdata %<>% filter(GEO_LEVEL=="Census subdivision") %>% filter(CHARACTERISTIC_ID%in%c(1:3))

data_qual <- popdata[,c(3,6)] %>% group_by(ALT_GEO_CODE) %>% summarise(`Quality Flags` = toString(DATA_QUALITY_FLAG))

popdata <- popdata[,-c(6,7,9,11)] %>% group_by(ALT_GEO_CODE) %>% pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL) %>% ungroup()

popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"),keep=F)
popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))
###### MAP DATA
# canada <- readOGR(dsn = "./ShapeFiles/Alt3/lcsd000b21a_e.shp", 
#                   stringsAsFactors = T)
canada <- read_sf(dsn = "./ShapeFiles/Alt3/TestReduced.shp", 
                  stringsAsFactors = T)
#Join map data to case data for easy ggplotting
canada <- left_join(canada,popdata, by=c("DGUID"="DGUID"),keep=F) 

PRUID <- factor(gsub("(^\\d{2}).*", "\\1", as.integer(as.character(popdata$ALT_GEO_CODE))))
popdata$PRUID <- PRUID
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

# for(i in 1:length(places)){
#   temp <- canada[gsub("(^\\d{2}).*", "\\1", as.integer(as.character(canada$CSDUID)))==13,]
#   ggplot()+
#     annotation_spatial(temp,lwd=0.05)+
#     layer_spatial(temp,aes(fill=`Population percentage change, 2016 to 2021`,color="Population not \nrecorded in 2016"),lwd=0.05)+
#     scale_colour_manual(values="black") +  
#     scale_fill_stepsn(breaks=c(-2.6,0,2.6,5.2,7.8,10.4,13),na.value="grey",
#                       colours =brewer.pal(9,"RdYlGn"),
#                       labels = c(-2.6,0,2.6,"5.2 (Canada)",7.8,10.4,13),
#                       limits=c(-5.2,15.6),
#                       oob=scales::squish,
#                       guide = guide_colourbar(ticks.colour="black",
#                                               barheight = 20,
#                                               frame.colour = "black"))+
#     labs(fill="% change in Pop, \n 2016-2021")+
#     theme(legend.title.align=0.5,
#           axis.line=element_blank(),  #bunch of options to remove "graph" visuals
#           axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks=element_blank(),
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank(),
#           panel.background=element_blank(),
#           panel.border=element_blank(),
#           panel.grid.major=element_blank(),
#           panel.grid.minor=element_blank(),
#           plot.background=element_blank(),
#           plot.title = element_text(hjust = 0.5,size=20),
#           legend.text=element_text(size=10),  #legend text sizes
#           legend.title=element_text(size=13))+
#     ggtitle(paste("Growth of\n",pnames[i],"\n2016-2021"))+
#     guides(colour=guide_legend("", override.aes=list(colour="black")))
#   
#   ggsave(paste0(sub("\\n.*","",pnames[i]),".pdf"),path="./Maps/")
# }

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
#paste0(sub("\\n.*","",pnames))
library(shiny)
library(shinyjs)
library(tmap)
pnames_simp <- data.frame(place = paste0(sub("\\n.*","",pnames)),
                          ID = as.integer(unique(canada$PRUID)))
pnames_simp <- pnames_simp[order(paste0(sub("\\n.*","",pnames))),]
names(canada)[11] <- "Name"
names(popdata)[5] <- "Name"
ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "province", 
                         label = "Select Provinces/Territories (More selected will take more time)",
                         choiceNames = pnames_simp[,1], 
                         choiceValues = pnames_simp[,2], 
                         inline = F),
      actionButton("submit", "Submit"),
      shinyjs::hidden(p("processing","Mapping..."))),
  mainPanel(tmapOutput(outputId = "canadamap"))),
  tableOutput("top"),
  tableOutput("bottom")
  #textOutput("test")
)
server <- function(input,output){
  

  observe({toggleState("submit",sum(input$province>0))})
  
  #output$test <- renderText(input$province)
  
  mapdata <- eventReactive(input$submit, {
    canada[as.numeric(canada$PRUID)%in%input$province,]
  })

  
  tabledatatop <- eventReactive(input$submit, {
    popdata[as.numeric(popdata$PRUID)%in%input$province,] %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(`Population percentage change, 2016 to 2021`) %>% 
      head(10) %>% 
      select(Name, 
             `Population, 2016`, 
             `Population, 2021`, 
             `Population percentage change, 2016 to 2021`, 
             `Quality Flags`)
  })
  tabledatabottom<- eventReactive(input$submit, {
    popdata[as.numeric(popdata$PRUID)%in%input$province,] %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(desc(`Population percentage change, 2016 to 2021`)) %>% 
      head(10) %>% 
      select(Name, 
             `Population, 2016`, 
             `Population, 2021`, 
             `Population percentage change, 2016 to 2021`, 
             `Quality Flags`)
  })
  
  output$canadamap <- renderTmap({
      tm_shape(st_make_valid(mapdata()))+
        tm_fill(col="Population percentage change, 2016 to 2021",
                alpha=0.5,
                midpoint = 5.2,
                breaks=c(-Inf,0,2.6,5.2,7.8,10.4,Inf),
                popup.vars=c("Name",
                             "Population, 2016",
                             "Population, 2021",
                             "Population percentage change, 2016 to 2021",
                             "Quality Flags"))+
        tm_borders()+
        tmap_options(check.and.fix = T,basemaps = "OpenStreetMap")+
        tm_view(leaflet.options = list("height"="1000"))
    
  })
  output$top <- renderTable({tabledatatop()})
  output$bottom <- renderTable({tabledatabottom()})
  
}

shinyApp(ui = ui, server = server)










# ui <- fluidPage(
#   checkboxGroupInput(inputId = "province", 
#                      label = "Select Provinces/Territories",
#                      choices = pnames_simp[,1], 
#                      selected = pnames_simp[9,1], 
#                      inline = F),
#   textOutput("test")
# )
# 
# server <- function(input,output){
#   output$test <- renderText(pnames_simp[pnames_simp[,1]%in%input$province,2])
#   
# }
# 
# shinyApp(ui = ui, server = server)