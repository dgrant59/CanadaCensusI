library(tidyverse)
library(readxl)
library(magrittr)
library(RColorBrewer)
library(sf)
library(shiny)
library(shinyjs)
library(leaflet)
library(leafpop)
library(bslib)

popdata <- read.csv("98-401-X2021006_English_CSV_data.csv",fileEncoding = "UTF-8")

#Filter only census subdivisions and 3 statistics relevant for plots
popdata %<>% filter(GEO_LEVEL=="Census subdivision") %>% filter(CHARACTERISTIC_ID%in%c(1:3))

#turn data quality flags into a single variable 0,0,0 is good
data_qual <- popdata[,c(3,6)] %>% 
              group_by(ALT_GEO_CODE) %>% 
                summarise(`Quality Flags` = toString(DATA_QUALITY_FLAG))

popdata <- popdata[,-c(6,7,9,11)] %>% 
            group_by(ALT_GEO_CODE) %>% 
              pivot_wider(names_from=CHARACTERISTIC_NAME,values_from=C1_COUNT_TOTAL) %>% 
                ungroup()

popdata <- left_join(popdata,data_qual,by=c("ALT_GEO_CODE"="ALT_GEO_CODE"),keep=F)


popdata %<>% mutate(ALT_GEO_CODE=factor(ALT_GEO_CODE))

###### MAP DATA
canada <- read_sf(dsn = "TestReduced.shp", 
                  stringsAsFactors = T)

#convert projection to standardized projection, WSG84
canada <- st_transform(canada, 4326)

#Join map data to case data for easy plotting
canada <- left_join(canada,popdata, by=c("DGUID"="DGUID"),keep=F) 

#ALT_GEO_CODE contains the province/territory for each subdivision as its first 2 digits, take these
#2 digits and make a new column
PRUID <- factor(gsub("(^\\d{2}).*", "\\1", as.integer(as.character(popdata$ALT_GEO_CODE)),useBytes = T))
popdata$PRUID <- PRUID

#Remove decimals from population counts
popdata %<>% mutate(`Population, 2016`= as.integer(`Population, 2016`))
popdata %<>% mutate(`Population, 2021`= as.integer(`Population, 2021`))

#These are the StatCan's province/territory IDs, will use these names/IDs as a key
#to match PRUID in popdata
#places <- c(10:13,24,35,46:48,59:62)
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
#Can quickly change between EN and FR by changing the definition in sub() below
pnames_simp <- data.frame(place = paste0(sub("\\n.*","",pnames)),
                          ID = as.numeric(levels(popdata$PRUID)))
#Associate PRUID with named province/territory
popdata %<>% mutate(PRName = factor(PRUID, labels = pnames_simp[,1]))
pnames_simp <- pnames_simp[order(paste0(sub("\\n.*","",pnames))),]

#Change census subdivision column name for nice output in leaflet popup
names(canada)[11] <- "Name"
names(popdata)[5] <- "Name"

##### LEAFLET MAP/SHINY APP PREREQS

#colour scheme for leaflet map, 5.2 (Canada average) is the middle point
#can change this scheme for accessibility if you want
colorfun <- colorBin("RdYlGn",bins=c(-Inf,0,2.6,5.2,7.8,10.4,Inf))

#Custom popup when you click on a specific subdivision on the map, derived from
#leafpop package output, but allows for finer tuning by editing the raw html.
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
#An empty table to go at the bottom of the app when the app starts
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


##### THE SHINY APP #####


ui <- fluidPage(
  includeCSS("www/style.css"),
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "#ED79F9", 
    base_font = "Arial"
  ),
  shinyjs::useShinyjs(),
  fluidRow(column(4,
                  tags$h1(style="text-align: center;",HTML("Population Changes in Canada<br/>2016 to 2021")),
                  tags$div(style="margin:auto;width='100%';text-align: center;",HTML("<small>Daniel Grant (see <a href='https://github.com/dgrant59/CanadaCensusI/'style='color:powderblue;'>github</a> for more info)</small>")),
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
           column(8,leafletOutput(outputId = "canadamap",width = "100%",height=675))),
  fluidRow(column(1),column(5,tags$h3("Top 5 Growing Regions"),tableOutput("top")),column(5, tags$h3("Bottom 5 Growing Regions"),tableOutput("bottom")),column(1))
)
server <- function(input,output,session){
  
  #Can't submit when nothing is selected
  observe({toggleState("submit",sum(input$province>0))})
  
  #Select all provinces with 1 button
  observe({
    if(is.null(input$s_all)||input$s_all==0){
      return()}
    else{updateCheckboxGroupInput(session,"province",selected=pnames_simp[,2])}
  })
  
  #Deselect all provinces with 1 button
  observe({
    if(is.null(input$d_all)||input$d_all==0){
      return()}
    else{updateCheckboxGroupInput(session,"province",selected=NULL,choiceNames = pnames_simp[,1], 
                                  choiceValues = pnames_simp[,2])}
  })
  #Select map data based on submitted provinces/territories
  mapdata <- eventReactive(input$submit, {
    canada[canada$PRUID%in%input$province,]
  })
  
  #select popdata based on submitted provinces/territories
  #'canada' has a format that needs to be converted for tables below to work
  #making popdata once instead of converting mapdata for each of 3 tables is 
  #probably more efficient
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
  
  output$canadamap <- renderLeaflet({leaflet(mapdata()) %>% 
      addPolygons(fillColor = ~colorfun(`Population percentage change, 2016 to 2021`),
                  fillOpacity = 0.6,
                  color="black",
                  weight=1,
                  opacity=1,
                  popup=customtable(popdataR())) %>% 
          addProviderTiles(providers$OpenStreetMap) %>% 
            addLegend("topright", 
                      title = "Population Change (%), 2016 to 2021",
                      colors=c(brewer.pal(6,"RdYlGn"),"#808080"), 
                      opacity=0.6,
                      values=~`Population percentage change, 2016 to 2021`,
                      labels = c("Less than 0","0 to 2.6","2.6 to 5.2","5.2 to 7.8",
                                 "7.8 to 10.4","More than 10.4","No/Missing population in 2016"))
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