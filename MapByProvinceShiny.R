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
#Join map data to case data for easy plotting
canada <- left_join(canada,popdata, by=c("DGUID"="DGUID"),keep=F) 

PRUID <- factor(gsub("(^\\d{2}).*", "\\1", as.integer(as.character(popdata$ALT_GEO_CODE))))
popdata$PRUID <- PRUID
popdata %<>% mutate(`Population, 2016`= as.integer(`Population, 2016`))
popdata %<>% mutate(`Population, 2021`= as.integer(`Population, 2021`))

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
library(shiny)
library(shinyjs)
library(tmap)
pnames_simp <- data.frame(place = paste0(sub("\\n.*","",pnames)),
                          ID =as.numeric(levels(popdata$PRUID)))
popdata %<>% mutate(PRName = factor(PRUID, labels = pnames_simp[,1]))
pnames_simp <- pnames_simp[order(paste0(sub("\\n.*","",pnames))),]
names(canada)[11] <- "Name"
names(popdata)[5] <- "Name"
ui <- fluidPage(
  tags$h1("Title"),
  tags$head(tags$style(
    HTML('
         #provpan {
            background-color: #ffffff;
            opacity: 0.2;
        }'))),
  shinyjs::useShinyjs(),
    tmapOutput(outputId = "canadamap",width = "100%",height=600),
    absolutePanel(id = "provpan",class = "panel panel-default",
                  draggable = TRUE, top = 240, left = 20, right = "auto", bottom = "auto",
                  width = 330, height = "auto",
                  checkboxGroupInput(inputId = "province", 
                       label = "Select Provinces/Territories (More selected will take more time)",
                       choiceNames = pnames_simp[,1], 
                       choiceValues = pnames_simp[,2], 
                       inline = F),
    actionButton("submit", "Submit")),
  # fluidRow(column(6,tableOutput("top")),column(6, tableOutput("bottom")))
)
server <- function(input,output){
  

  observe({toggleState("submit",sum(input$province>0))})
  
  mapdata <- eventReactive(input$submit, {
    canada[canada$PRUID%in%input$province,]
  })

  
  tabledatatop <- eventReactive(input$submit, {
    popdata[popdata$PRUID%in%input$province,] %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(`Population percentage change, 2016 to 2021`) %>% 
      head(5) %>% 
      select(Name,
             PRName,
             `Population, 2016`, 
             `Population, 2021`, 
             `Population percentage change, 2016 to 2021`, 
             `Quality Flags`)
  })
  tabledatabottom<- eventReactive(input$submit, {
    popdata[popdata$PRUID%in%input$province,] %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(desc(`Population percentage change, 2016 to 2021`)) %>% 
      head(5) %>% 
      select(Name, 
             PRName,
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
  output$top <- renderTable({tabledatatop()},striped = T)
  output$bottom <- renderTable({tabledatabottom()},striped=T)
  
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