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
library(bslib)
#setwd("PATHHERE")


popdata <- read.csv("98-401-X2021006_English_CSV_data.csv")
popdata %<>% filter(GEO_LEVEL=="Census subdivision") %>% filter(CHARACTERISTIC_ID%in%c(1:3))

data_qual <- popdata[,c(3,6)] %>% group_by(ALT_GEO_CODE) %>% summarise(`Quality Flags` = toString(DATA_QUALITY_FLAG))

popdata <- popdata[,-c(6,7,9,11)] %>% group_by(ALT_GEO_CODE) %>% pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL) %>% ungroup()

popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"),keep=F)
popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))
###### MAP DATA
canada <- read_sf(dsn = "TestReduced.shp", 
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

dummytable <- popdata %>%
  head(5) %>% 
  select(Name,
         PRName,
         `Population, 2016`, 
         `Population, 2021`, 
         `Population percentage change, 2016 to 2021`, 
         `Quality Flags`)
names(dummytable)[c(2,5)] <- c("Province / Territory","Population Change (%), 2016 to 2021")
for(i in 1:6){
  dummytable[,i]=rep("",5)
}



ui <- fluidPage(
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "#ED79F9", 
    base_font = "Arial"
  ),
  tags$head(includeCSS("./www/style.css")),
  shinyjs::useShinyjs(),
  fluidRow(column(4,
                  tags$h1(style="text-align: center;",HTML("Population Changes in Canada<br/>2016 to 2021")),
                  tags$div(style="margin:auto;width='100%';text-align: center;",tags$small("Daniel Grant (see github for more info)")),
                  tags$h4(style="text-align: center;","Select Provinces/Territories"),
                  tags$h5(style="text-align: center;","(More selected will take more time)"),
                  br(),
                  checkboxGroupInput(inputId = "province", 
                                     choiceNames = pnames_simp[,1], 
                                     choiceValues = pnames_simp[,2], 
                                     label=NULL,
                                     inline = F,
                                     width="70%"),
                  br(),
                  actionButton("s_all", "Select All",width="49%"),
                  actionButton("d_all", "Deselect All",width="49%"),
                  br(),
                  br(),
                  actionButton("submit", "Submit",width="99%")),
           column(8,leafletOutput(outputId = "canadamap",width = "100%",height=600))),
  fluidRow(column(1),column(5,tags$h3("Top 5 Growing Regions"),tableOutput("top")),column(5, tags$h3("Bottom 5 Growing Regions"),tableOutput("bottom")),column(1))
)
server <- function(input,output,session){
  

  observe({toggleState("submit",sum(input$province>0))})
  
  observe({
    if(is.null(input$s_all)||input$s_all==0)
      return()
    else{updateCheckboxGroupInput(session,"province",selected=pnames_simp[,2])}
  })
  observe({
    if(is.null(input$d_all)||input$d_all==0)
      return()
    else{updateCheckboxGroupInput(session,"province",selected=NULL,choiceNames = pnames_simp[,1], 
                                  choiceValues = pnames_simp[,2])}
  })
  mapdata <- eventReactive(input$submit, {
    canada[canada$PRUID%in%input$province,]
  })
  
  popdataR <- eventReactive(input$submit, {
    popdata[popdata$PRUID%in%input$province,]
  })
  tabledatabottom <- eventReactive(input$submit, {
    x <- popdataR() %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(`Population percentage change, 2016 to 2021`) %>% 
      head(5) %>% 
      select(Name,
             PRName,
             `Population, 2016`, 
             `Population, 2021`, 
             `Population percentage change, 2016 to 2021`) %>%
      mutate(`Population, 2016` = format(`Population, 2016`,big.mark=",")) %>%
      mutate(`Population, 2021` = format(`Population, 2021`,big.mark=","))
    names(x)[c(2,5)] <- c("Province / Territory","Population Change (%), 2016 to 2021")
    x
  })
  tabledatatop<- eventReactive(input$submit, {
    x <- popdataR() %>% 
      filter(`Population, 2016`>=1000)%>%
      arrange(desc(`Population percentage change, 2016 to 2021`)) %>% 
      head(5) %>% 
      select(Name, 
             PRName,
             `Population, 2016`, 
             `Population, 2021`, 
             `Population percentage change, 2016 to 2021`) %>%
      mutate(`Population, 2016` = format(`Population, 2016`,big.mark=",")) %>%
      mutate(`Population, 2021` = format(`Population, 2021`,big.mark=","))
    names(x)[c(2,5)] <- c("Province / Territory","Population Change (%), 2016 to 2021")
    x
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

  output$top <- renderTable({if(is.null(input$submit)||input$submit==0){
    dummytable
  }
    else{tabledatatop()}},striped = T)
  output$bottom <- renderTable({if(is.null(input$submit)||input$submit==0){
    dummytable
  }
    else{tabledatabottom()}},striped = T)
  
}

shinyApp(ui = ui, server = server)



  
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