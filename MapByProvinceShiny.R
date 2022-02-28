library(tidyverse)
library(readxl)
library(magrittr)
library(tmap)
library(RColorBrewer)
library(svglite)
library(sf)
library(shiny)
library(shinyjs)
library(tmap)
library(leaflet)
library(leafpop)
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
canada <- st_transform(canada, 4326)
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
pnames_simp <- data.frame(place = paste0(sub("\\n.*","",pnames)),
                          ID =as.numeric(levels(popdata$PRUID)))
popdata %<>% mutate(PRName = factor(PRUID, labels = pnames_simp[,1]))
pnames_simp <- pnames_simp[order(paste0(sub("\\n.*","",pnames))),]
names(canada)[11] <- "Name"
names(popdata)[5] <- "Name"

colorfun <- colorBin("RdYlGn",bins=c(-Inf,0,2.6,5.2,7.8,10.4,Inf))

ui <- fluidPage(
  tags$h1("Title"),
  tags$head(includeCSS("./www/style.css")),
  shinyjs::useShinyjs(),
  sidebarLayout(sidebarPanel(id = "provpan",class = "panel panel-default",
                             checkboxGroupInput(inputId = "province", 
                                                label = "Select Provinces/Territories (More selected will take more time)",
                                                choiceNames = pnames_simp[,1], 
                                                choiceValues = pnames_simp[,2], 
                                                inline = F),
                             actionButton("submit", "Submit")),
                mainPanel(leafletOutput(outputId = "canadamap",width = "100%",height=600))),
  fluidRow(column(6,tableOutput("top")),column(6, tableOutput("bottom")))
)
server <- function(input,output){
  

  observe({toggleState("submit",sum(input$province>0))})
  
  mapdata <- eventReactive(input$submit, {
    canada[canada$PRUID%in%input$province,]
  })
  
  popdataR <- eventReactive(input$submit, {
    popdata[popdata$PRUID%in%input$province,]
  })
  tabledatatop <- eventReactive(input$submit, {
    popdataR() %>% 
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
    popdataR() %>% 
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
  
  output$canadamap <- renderLeaflet({leaflet(mapdata()) %>% addPolygons(
    fillColor = ~colorfun(`Population percentage change, 2016 to 2021`),
    fillOpacity = 0.6,
    color="black",
    weight=1,
    opacity=1,
    popup=customtable(popdataR())
  ) %>% addProviderTiles(providers$OpenStreetMap) %>% addLegend("topright", 
                                                                title = "Population Change (%), 2016 to 2021",
                                                                colors=c(brewer.pal(6,"RdYlGn"),"#808080"), 
                                                                opacity=0.6,
                                                                values=~`Population percentage change, 2016 to 2021`,
                                                                labels = c("Less than 0","0 to 2.6","2.6 to 5.2","5.2 to 7.8","7.8 to 10.4","More than 10.4","No population in 2016"))
  })

  output$top <- renderTable({tabledatatop()},striped = T)
  output$bottom <- renderTable({tabledatabottom()},striped=T)
  
}

shinyApp(ui = ui, server = server)


#derived from leafpop 
customtable <- function(data){
  paste0(
    "<div class='scrollableContainer'>
      <table class='datapopup'>
      <tr>
      <td></td>
      <th style='font-weight:bold'>Name&emsp;</th>
      <td style='text-align:right'>",data$Name,"&emsp;</td>
      </tr>
      <tr>
         <td></td>
         <th style='font-weight:bold'>Population, 2016&emsp;</th>
         <td style='text-align:right'>",format(data$`Population, 2016`,big.mark=","),"&emsp;</td>
      </tr>
      <tr>
         <td></td>
         <th style='font-weight:bold'>Population, 2021&emsp;</th>
         <td style='text-align:right'>",format(data$`Population, 2021`,big.mark=","),"&emsp;</td>
      </tr>
      <tr>
         <td></td>
         <th style='font-weight:bold'>Population Change (%), 2016 to 2021&emsp;</th>
         <td style='text-align:right'>", data$`Population percentage change, 2016 to 2021`,"&emsp;</td>
      </tr>
      <tr>
         <td></td>
         <th style='font-weight:bold'>Quality Flags&emsp;</th>
         <td style='text-align:right'>",data$`Quality Flags`,"&emsp;</td>
      </tr>
   </table>
</div>"
  )
}  
  
# "
# <div class='scrollableContainer'>
#    <table class= id='popup'>
#       <tr>
#          <td></td>
#          <th>Name&emsp;</th>
#          <td>St. Shott's, Town (T)&emsp;</td>
#       </tr>
#       <tr>
#          <td></td>
#          <th>Population, 2016&emsp;</th>
#          <td>    66&emsp;</td>
#       </tr>
#       <tr>
#          <td></td>
#          <th>Population, 2021&emsp;</th>
#          <td>    55&emsp;</td>
#       </tr>
#       <tr>
#          <td></td>
#          <th>Population percentage change, 2016 to 2021&emsp;</th>
#          <td> -16.7&emsp;</td>
#       </tr>
#       <tr>
#          <td></td>
#          <th>Quality Flags&emsp;</th>
#          <td>0, 0, 0&emsp;</td>
#       </tr>
#       <tr>
#          <td></td>
#          <th>geometry&emsp;</th>
#          <td>sfc_MULTIPOLYGON&emsp;</td>
#       </tr>
#    </table>
# </div>
# "





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