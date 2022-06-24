#### BRING IN DATASET ####

## Load packages
library(shiny)
library(mapview)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(sf)
library (RPostgres)
library(DBI)
library(plyr)
library(dplyr)
library(pool)
#library(geojsonio)
library(sp)
library(RPostgreSQL)
library(glue)
library(postGIStools)
library(config)
library(geojsonio)
library(shinybusy)

## Stop for 502 error
refresh()
######################################

## Add busy spinner from shinybusy package
#https://dreamrs.github.io/shinybusy/
#https://cran.r-project.org/web/packages/shinybusy/shinybusy.pdf
add_busy_spinner(spin = "fading-circle",position = "bottom-left",margins = c(40, 60),color = "#044D94",timeout = 300,height = "70px",width = "70px")##FFFFFF #0D4581

##############################
#__________________________________________________________________________________________
#### CODE TO SOLVE ERROR: Missing dbQuoteLiteral methods for pool'####

##https://github.com/rstudio/pool/issues/96
#' @importMethodsFrom DBI dbQuoteLiteral
#' @importFrom pool poolCheckout poolReturn
#' @export
setMethod("dbQuoteLiteral", c("Pool", "ANY"),
          function(conn, x, ...) {
            # As of 2020-05-07, this is necessary due to an incompatiblity
            # between packages `pool` (v 0.1.4.3) and `glue` (v >= 1.3.2).
            # In v1.3.2, `glue` started using `dbQuoteLiteral`, which is
            # currently not offered by `pool`, so creating it here.
            connection <- pool::poolCheckout(conn)
            on.exit(pool::poolReturn(connection))
            DBI::dbQuoteLiteral(connection, x, ...)
          }
)
#__________________________________________________________________________________________
#### CREATE A CONNECTION TO OneBenthic trawl LIVE ####
Sys.setenv(R_CONFIG_ACTIVE = "one_benthic_trawl")

dw <- config::get()

pool <- dbPool(drv = dbDriver(dw$driver),
               dbname = dw$database,
               host = dw$server,
               port =  dw$port,
               user = dw$uid,
               password = dw$pwd)
#__________________________________________________________________________________________
#### GET POINT SAMPLE DATA (META & FAUNA) ####

## Minimal data for map and drop down selection
data <- dbGetQuery(pool, "SELECT
                       s.samplecode,
                       s.date,
                       s gear,
                       s.samplelatstart,
                       s.samplelongstart,
                       ss.survey_surveyname
                       FROM
                       samples.sample as s
                       INNER JOIN associations.surveysample as ss ON s.samplecode = ss.sample_samplecode")


## Drop down choices for input$inputSurvey
surveys <- sort(unique(data$survey_surveyname))

## Map data
points <- unique(data[,c(5:4,6,1)])
colnames(points)[1] <- "Longitude"
colnames(points)[2] <- "Latitude"
colnames(points)[3] <- "SurveyName"
colnames(points)[4] <- "SampleCode"

#__________________________________________________________________________________________
#### GET SPATIAL LAYERS VIA API (AGG, OWF, WAVE, TIDE) ####
## 29/04/2020 Stopped using api as addresses kept changing
#https://opendata.arcgis.com/datasets/4da955de094e475d8d902ee446e38d58_0.geojson
## Bring in data via API
#agg <- readLines("https://opendata.arcgis.com/datasets/ced5788f014546b0b571e8d29b021166_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#owf <- readLines("https://opendata.arcgis.com/datasets/4da955de094e475d8d902ee446e38d58_0.geojson")%>% paste(collapse = "\n") %>% geojson_sf()
#owf_cab<- readLines("https://opendata.arcgis.com/datasets/c42f90c49f0d44f0997eaa52d692be3d_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#wave <- readLines("https://opendata.arcgis.com/datasets/d9f9dbc0e29b410c87ba544f6082a0d0_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#wave_cab <- readLines("https://opendata.arcgis.com/datasets/00ab10c847fb42a9bef8f0f4644c41ac_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#tidal <- readLines("https://opendata.arcgis.com/datasets/db94124b152641a992d4e8dfa14d59f2_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#tidal_cab <- readLines("https://opendata.arcgis.com/datasets/3e5203ce7daa4ae08690699925668f46_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#R4_chara <- readLines("https://opendata.arcgis.com/datasets/c0e61d8972e4438ab1c39304b7f28608_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#R4_bid <- readLines("https://opendata.arcgis.com/datasets/54dce8a263324a85b36523e31fff20cc_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#oga <- readLines("https://opendata.arcgis.com/datasets/3c950a2c8186438899f99ced733dd947_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()

## Check classof spatial objects
#class(agg) #"sf"         "data.frame"
#class(owf)#"sf"         "data.frame"
#class(owf_cab)#"sf"         "data.frame"
#class(wave)#"sf"         "data.frame"
#class(wave_cab)#"sf"         "data.frame"
#class(tidal)#"sf"         "data.frame"
#class(tidal_cab)#"sf"         "data.frame"
#class(R4_chara)#"sf"         "data.frame"
#class(R4_bid)#"sf"         "data.frame"

## Check CRS of spatial objects
#st_crs(agg) #EPSG: 4326 
#st_crs(owf) #EPSG: 4326 
#st_crs(owf_cab) #EPSG: 4326 
#st_crs(wave) #EPSG: 4326 
#st_crs(wave_cab) #EPSG: 4326 
#st_crs(tidal) #EPSG: 4326 
#st_crs(tidal_cab) #EPSG: 4326 
#st_crs(R4_chara) #EPSG: 4326 
#st_crs(R4_bid) #EPSG: 4326 
#__________________________________________________________________________________________
#### CREATE A CONNECTION TO OneBenthic LIVE ####
Sys.setenv(R_CONFIG_ACTIVE = "one_benthic")

dw <- config::get()

pool2 <- dbPool(drv = dbDriver(dw$driver2),
               dbname = dw$database2,
               host = dw$server2,
               port =  dw$port2,
               user = dw$uid2,
               password = dw$pwd2)
#__________________________________________________________________________________________
#### BRING IN ACTIVITY LAYERS ####

## GET API LINKS FROM OB
euowfapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=1") 
owfapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=2")
owf_cabapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=3")
waveapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=4")
tidalapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=5")
tidal_cabapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=6")
R4_charaapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=7")
R4_bidapilink <- st_read (dsn = pool2, query = "SELECT apilink FROM spatial.apilinks where id=8")

## API
euowf <- readLines(as.character(euowfapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
owf <- readLines(as.character(owfapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
owf_cab <- readLines(as.character(owf_cabapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
wave <- readLines(as.character(waveapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
tidal <- readLines(as.character(tidalapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
tidal_cab <- readLines(as.character(tidal_cabapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
R4_chara <- readLines(as.character(R4_charaapilink)) %>% paste(collapse = "\n") %>% geojson_sf()
R4_bid <- readLines(as.character(R4_bidapilink)) %>% paste(collapse = "\n") %>% geojson_sf()

## SPATIAL DATA FROM ONEBENTHIC
oga <- st_read(pool2, query = "SELECT * FROM spatial.oga_licences_wgs84;")
mcz <-  st_read(pool2, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'MCZ - Secretary of State';")
sac <-  st_read(pool2, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'SAC'or site_statu = 'cSAC';")
ncmpa <-  st_read(pool2, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'NCMPA';")
disp  <-  st_read(pool2, query = "SELECT * FROM spatial.disposalSiteJan2020;")
#ref <- st_read(pool2, query = "SELECT * FROM spatial.ref_box_all;")
#siz<- st_read(pool2, query = "SELECT * FROM ap_marine_aggregate.extraction_areas_siz;")
agg <- st_read(pool2, query = "SELECT * FROM ap_marine_aggregate.extraction_areas;")

## Check CRS
st_crs(mcz)#Coordinate Reference System: NA
st_crs(sac)#Coordinate Reference System: NA
st_crs(ncmpa)#Coordinate Reference System: NA
st_crs(oga)#Coordinate Reference System: NA
st_crs(disp)#Coordinate Reference System: NA
st_crs(agg) # 4326
st_crs(owf)#Coordinate Reference System: NA
st_crs(owf_cab)#Coordinate Reference System: NA
st_crs(wave)#Coordinate Reference System: NA
st_crs(tidal)#Coordinate Reference System: NA
st_crs(tidal_cab)#Coordinate Reference System: NA
st_crs(R4_chara)#Coordinate Reference System: NA
st_crs(R4_bid)#Coordinate Reference System: NA

## Set CRS where necessary
st_crs(mcz) <- 4326
st_crs(sac) <- 4326
st_crs(ncmpa) <- 4326
st_crs(disp) <- 4326

#__________________________________________________________________________________________
#### MAP LAYERS SOURCE INFO ####
map_overlays <- data.frame(
  Code=c("euowf",
               "owf",
               "owf_cab",
               "R4_chara",
               "R4_bid",
               "agg",
               "disp",
               "wave",
               #"wave_cab",
               "tidal",
               "tidal_cab",
               "oga",
               "mcz",
               "sac",
               "ncmpa"),
  Link=c('<p><a href="https://www.emodnet-humanactivities.eu/search-results.php?dataname=Wind+Farms+%28Polygons%29"
      >Wind Farms (Polygons)</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-wind-site-agreements-england-wales-ni-the-crown-estate/explore?location=52.790200%2C-1.251504%2C7.36"
      >Offshore Wind Site Agreements (England, Wales & NI), The Crown Estate</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-wind-cable-agreements-england-wales-ni-the-crown-estate/explore?location=52.588556%2C-1.244512%2C7.00"
      >Offshore Wind Cable Agreements (England, Wales & NI), The Crown Estate</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-wind-leasing-round-4-characterisation-areas-england-wales-and-ni-the-crown-estate/explore?location=52.677790%2C-1.394816%2C7.18"
     >Offshore Wind Leasing Round 4 Characterisation Areas (England, Wales and NI), The Crown Estate</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-wind-leasing-round-4-bidding-areas-england-wales-and-ni-the-crown-estate/explore?location=52.930510%2C-0.830858%2C7.00"
      >Offshore Wind Leasing Round 4 Bidding Areas (England, Wales and NI), The Crown Estate</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-minerals-aggregates-site-agreements-england-wales-ni-the-crown-estate/explore?location=52.033181%2C-1.121135%2C7.89"
      >Offshore Minerals Aggregates Site Agreements (England, Wales & NI), The Crown Estate</a></p>',
         '<p><a href="http://data.cefas.co.uk/#/View/407"
      >UK Disposal Site Layer, Cefas</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-wave-site-agreements-england-wales-ni-the-crown-estate/explore?location=50.777918%2C-5.092345%2C9.26"
     >Offshore Wave Site Agreements (England, Wales & NI), The Crown Estate</a></p>',
         #'<p><a href="https://opendata.arcgis.com/datasets/bf376b05c6ae489b8b8687d6b7d6525d_0.geojson">Visit W3Schools.com!</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-tidal-stream-site-agreements-england-wales-ni-the-crown-estate/explore?location=52.888850%2C-3.683844%2C7.42"
      >Offshore Tidal Stream Site Agreements (England, Wales & NI), The Crown Estate</a></p>',
         '<p><a href="https://opendata-thecrownestate.opendata.arcgis.com/datasets/offshore-tidal-stream-cable-agreements-england-wales-ni-the-crown-estate/explore?location=51.877184%2C-5.315998%2C17.04"
      >Offshore Tidal Stream Cable Agreements (England, Wales & NI), The Crown Estate</a></p>',
         '<p><a href="https://data-ogauthority.opendata.arcgis.com/datasets/oga-licences-wgs84-3/explore?location=56.616000%2C-5.050750%2C5.16"
      >OGA Licences WGS84, Oil and Gas Authority</a></p>',
         #'<p><a href="https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp"
         '<p><a href="https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27"
          >Marine Conservation Zones (MCZ)</a></p>',
         '<p><a href="https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27"
      >Special Area of Conservation (SAC)</a></p>',
         '<p><a href="https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27"
      >Nature Conservation Marine Protected Areas (Scotland)</a></p>'),
  Description=c("Offshore wind installations in European seas from the European Marine Observation and Data Network (EMODnet)",
          "This dataset represents all current offshore wind farm agreements in pre-planning, planning, construction and operational phases, as well as Preferred Projects subject to HRA, in English, Welsh and Northern Irish waters.",
          "This dataset represents all current export cables for offshore wind farm agreements in pre-planning, planning, construction and operational phases in English, Welsh and Northern Irish waters.",
          "This dataset represents areas of seabed defined by The Crown Estate within each of the Bidding Areas which are considered to present the greatest opportunity to Bidders based on thorough assessment of the constraints.",
          "This dataset represents the external boundary of the areas of seabed within which Bidders can propose projects through the Round 4 leasing process.",
          "This dataset represents all current marine aggregates site agreements in English, Welsh and Northern Irish waters.",
          "UK Disposal Sites (layer maintained by Cefas)",
          "This dataset represents all current wave agreements in English, Welsh and Northern Irish waters.",
          #"This dataset represents all current wave agreements in English, Welsh and Northern Irish waters.",
          "This dataset represents all current tidal stream agreements in English, Welsh and Northern Irish waters",
          "This dataset represents all current export cables for tidal stream agreements in English, Welsh and Northern Irish waters.",
          "OGA Licences WGS84, Oil and Gas Authority",
          "Marine Conservation Zones (MCZ)",
          "Special Area of Conservation (SAC)",
          "Nature Conservation Marine Protected Areas (Scotland)"))


#__________________________________________________________________________________________
#### USER INTERFACE ####

ui <- fluidPage(add_busy_spinner(spin = "fading-circle",position = "bottom-left",margins = c(40, 60),color = "#044D94",timeout = 300,height = "70px",width = "70px"),
  titlePanel(title=div(img(src="onebenthic.gif",tags$b(" OneBenthic"),"Data Extraction Tool: Trawl",height = 70, width = 170),#HEIGHT65
                       style='background-color:#B4C7E7;
                    padding-right: 50px')),
  
  fluidRow(
    
    #__________________________________________________________________________________________
    #### SELECT SURVEY(S) ####    
    column(2,selectInput(inputId="surveyInput", multiple = T,h4("Select by:",br(),br(),"1. Survey",style="color:#808080"),choices =surveys),h4("Or",br(),br()," 2. Sample (using map tool",style="color:#808080",img(src="drawrectangleicon.png",height = 20, width = 20),")",br(),br(),br(),br(),br(),br(),"Searches independent - see appropriate tab for results",br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),tags$i("'Use the app to explore and access benthic data from 2m beam trawl samples...'"))),
    
    #__________________________________________________________________________________________
    #### MAP ####    
    column(5,leafletOutput("map",width = "100%", height=830),style='border-left: 1px solid grey'),#850
    column(5,style='border-left: 1px solid grey',
           
           #__________________________________________________________________________________________
           #### TAB: DATA BY SURVEY ####
           tabsetPanel(             
             tabPanel("Data (by Survey)",div(DT::dataTableOutput("coordinates"),style = 'font-size:85%'),br(),
                      downloadButton("downloadData", "Download data")),
             #__________________________________________________________________________________________
             #### TAB: DATA BY SAMPLE ####
             tabPanel("Data (by Sample)",div(DT::dataTableOutput("bbdata"),style = 'font-size:85%'),br(),
                      downloadButton("downloadData2", "Download data")),
             #__________________________________________________________________________________________
             #### TAB: ABOUT  ####
             tabPanel("About",
                      
                      h4("App purpose"),"
To allow users to explore and download trawl sample data (macrofaunal abundances/biomass) from the",tags$b("OneBenthic")," Trawl (OBT) sample database. Select by",tags$b("survey")," (drop-down list), or",tags$b("sample"),"(map drawing tool). Selected samples are shown in a table (see appropriate tab), with data available for csv download. Where data providers withold permission, only sample metadata will be output. Details of the activity layers shown in the map can be found in the 'Map Overlays' tab.",br(), 
                      h4("What is OneBenthic?"),"Large quantities of benthic data are now in the public domain, yet are distributed across multiple repositories. ", "The purpose of the ",tags$b("OneBenthic"), "big data initiative is to bring these disperate datasets together in one place, thereby aiding research and facilitating reuse of data. Big data are essential for addressing key questions concerning biodiversity, marine spatial planning, conservation, climate change and cumulative effects. They can also facilitate differrent ways of working, helping to reduce the costs of environmental assessment for seabed users and developers. Taxon names are standardized according to the", tags$a(href="http://www.marinespecies.org/", "World Register of Marine Species."),"The ",tags$b("OneBenthic")," databases for grab/core and trawl sample data link directly to a range of other apps providing useable information back to data providers (and the wider public). All apps are available from",tags$a(href="https://openscience.cefas.co.uk/", "CefasOpenScience,"),
                      "These tools are actively being used by offshore marine industries and government for purposes of project planning, licence compliance monitoring and research.",br(),
                      h4("Origin of data"),"Data originate from multiple sources and providers (see 'Data Provider' tab). Subject to funding, new publicly available data will continue to be added to",tags$b("OneBenthic")," so that it continues to provide an up-to-date and 'complete' source of UK benthic data.",br(),
                      h4("Data access and use"),tags$b("OneBenthic"),"contains data from a range of data providers in industry and government. With the permission of these providers, data are made available under Crown Copyright and", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", "Open Government Licence."),"
Please cite the database as follows: ",br(),tags$b("OneBenthic")," database (2022). Available from https://rconnect.cefas.co.uk/onebenthic_dataextractiontrawl/. Accessed: DD/MM/YYYY.",br(),
                      h4("Data Disclaimer"),"Whilst due care and attention has been exercised in the collation of",tags$b("OneBenthic"),"data, Cefas assumes no responsibility for the quality or accuracy of the information. Users are advised to check data with the original source, and to critically assess whether data are fit for the user's intended purpose.",br(),
                      h4("Contact"),"Get in touch to tell us how you've used",tags$b("OneBenthic"),"data, to share new insights with the",tags$b("OneBenthic"),"community or to report technical or data issues. Email: keith.cooper@cefas.co.uk",
                      style = 'font-size:90%'),
             
             #__________________________________________________________________________________________
             #### TAB: MAP OVERLAYS ####
             #tabPanel("Map Layers",br(),DT::dataTableOutput("activitytable"),style = 'font-size:85%'),
             tabPanel("Map Overlays",br(),htmlOutput("activitytable"),style = 'font-size:80%'),
             
             #__________________________________________________________________________________________
             #### TAB: DATA PROVIDERS ####
             tabPanel("Data Providers",br(),"We gratefully acknowledge all the individual data providers:",br(),br(),
                      h4("Government"), tags$i("Department for Environment, Food and Rural Affairs (DEFRA); Centre for Environment, Fisheries and Aquaculture Science (CEFAS)"),br(),br(),
                      h4("Marine Aggregates"),tags$i("Hanson Aggregates Marine Limited"),br(),br(),
                      h4("Offshore Wind"), tags$i("SMart Wind Limited"),br(),br(),
                      #h4("Oil & Gas"), br(),br(),
                      h4("Nuclear"),tags$i("EDF Energy"),br(),br(),
                      #h4("Ports & Harbours"),
                      style = 'font-size:90%'),
             
             #__________________________________________________________________________________________
             #### TAB: FUNDERS ####
             
             tabPanel("Supporters",br(),tags$b("OneBenthic"),"is free to use, but not to run. If you found this app useful then please consider joining existing funders and partners to support the initiative. Thank you!",br(),(img(src="logos2.PNG")),style = 'font-size:90%')
           )
    )
  )
  #__________________________________________________________________________________________
  #### SUPRESS IRRELEVANT ERROR MESSAGE ON SAMPLE TAB ####
  ,tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
  )
  #__________________________________________________________________________________________
)

#__________________________________________________________________________________________
#### SERVER FUNCTION ####
server <- function(input, output) {
  #__________________________________________________________________________________________
  #### TABLE FOR ACTIVITY LAYERS ####
  
  library(knitr)
  library(kableExtra)

  output$activitytable <- renderText({

    kable(map_overlays, escape=FALSE,format = "html") %>%
      column_spec (1, bold = T)%>%
      kable_styling(bootstrap_options = c("striped","hover", "condensed"))#%>%
      #scroll_box( height = "900px")#width = "900px",

  })
  
  #__________________________________________________________________________________________  
  #### INTERACTIVE MAP ####
  output$map <- renderLeaflet({
    
    ## Basic map
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
      addPolygons(data=euowf,color = "#444444", weight = 1, smoothFactor = 0.5,group = "euowf",popup = paste0("<b>Name: </b>", euowf$name))%>%
      addPolygons(data=owf,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf",popup = paste0("<b>Name: </b>", owf$Name_Prop, "<br>","<b>Status: </b>", owf$Inf_Status))%>%
      addPolygons(data=owf_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf_cab",popup = paste0("<b>Name: </b>", owf_cab$Name_Prop, "<br>","<b>Status: </b>", owf_cab$Infra_Stat))%>%
      addPolygons(data=R4_chara,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_chara",popup = paste0("<b>Name: </b>", R4_chara$Name))%>%
      addPolygons(data=R4_bid,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_bid",popup = paste0("<b>Name: </b>", R4_bid$Name, "<br>","<b>Status: </b>", R4_bid$Bidding_Ar))%>%
      addPolygons(data=agg,color = "#444444", weight = 1, smoothFactor = 0.5,group = "agg",popup = paste0("<b>Name: </b>", agg$Area_Name, "<br>","<b>Number: </b>", agg$Area_Numbe))%>%
      addPolygons(data=disp,color = "#444444", weight = 1, smoothFactor = 0.5,group = "disp",popup = paste0("<b>Name: </b>", disp$name_, "<br>","<b>Number: </b>", disp$site_))%>%
      addPolygons(data=wave,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave",popup = paste0("<b>Name: </b>", wave$Name_Prop, "<br>","<b>Status: </b>", wave$Inf_Status))%>%
      #addPolygons(data=wave_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave_cab",popup = paste0("<b>Name: </b>", wave_cab$name_prop, "<br>","<b>Status: </b>", wave_cab$infra_stat))%>%
      addPolygons(data=tidal,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal",popup = paste0("<b>Name: </b>", tidal$Name_Prop, "<br>","<b>Status: </b>", tidal$Inf_Status))%>%
      addPolygons(data=tidal_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal_cab",popup = paste0("<b>Name: </b>", tidal_cab$Name_Prop, "<br>","<b>Status: </b>", tidal_cab$Infra_Stat))%>%
      addPolygons(data=mcz,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "mcz",popup = paste0("<b>Name: </b>", mcz$site_name))%>%
      addPolygons(data=sac,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "sac",popup = paste0("<b>Name: </b>", sac$site_name))%>%
      addPolygons(data=ncmpa,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "ncmpa",popup = paste0("<b>Name: </b>", ncmpa$site_name))%>%
      addPolygons(data=oga,color = "#444444", weight = 1, smoothFactor = 0.5,group = "oga",popup = paste0("<b>Number: </b>", oga$LICREF, "<br>","<b>Organisation: </b>", oga$LICORGGR))%>%
      addLayersControl(
        overlayGroups = c("euowf","owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","tidal","tidal_cab","oga","mcz","sac","ncmpa"),options = layersControlOptions(collapsed = FALSE))%>%#"wave_cab",
      hideGroup(c("euowf","owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","tidal","tidal_cab","oga","mcz","sac","ncmpa"))%>%#"wave_cab",
      addCircleMarkers(data=points,~Longitude,~Latitude,radius = 2,stroke = FALSE,fillOpacity = 0.4,popup = paste0("<b>Survey Name: </b>",points$SurveyName,"<br>","<b>Sample Code: </b>",points$SampleCode))%>%
      addCircleMarkers(data=points,~Longitude,~Latitude,radius = 2,stroke = FALSE,fillOpacity = 0.4,group = "myMarkers")%>%
      addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,circleMarkerOptions = F, polygonOptions = F, singleFeature=TRUE)%>%
      setView(-3,54.6,zoom=5)%>%
      addMouseCoordinates()
    
  })
  #popup = ~as.character(SurveyName),
  #__________________________________________________________________________________________  
  #### UPDATE MAP WITH SELECTED SURVEYS ####
  #https://stackoverflow.com/questions/46979328/how-to-make-shiny-leaflet-map-reac-to-change-in-input-value-r
  
  # Watch for selection of new survey(s) 
  observeEvent(input$surveyInput, { 
    
    # Modify existing map
    leafletProxy("map") %>%
      
      # Remove any previous selections 
      clearGroup("myMarkers") %>%
      
      # Highlight new selected surveys
      #addMarkers(data = points[points$SurveyName == input$surveyInput, ],
      addMarkers(data = points[points$SurveyName %in% input$surveyInput, ],
                 ~Longitude,
                 ~Latitude,
                 #group = "myMarkers")
                
                 #group = "myMarkers",popup = ~paste(SurveyName,SampleCode))
                 group = "myMarkers",popup = ~paste("<b>Survey Name: </b>",SurveyName,"<br>","<b>Sample Code: </b>",SampleCode))
    #group = "myMarkers",popup = ~as.character("Survey Code:",points$SampleCode))#only gives header
    
    #group = "myMarkers",popup = paste0("Survey Name",points$SurveyName))# didn't work'
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT: SAMPLE METADATA FOR SELECTED SURVEY(S) ####
  
  coord <- reactive({
    req(input$surveyInput)
    
    coord2 <- glue::glue_sql(
      "SELECT su.surveyname,
s.samplecode,
s.samplelatstart,
s.samplelongstart,
s.samplelatend,
s.samplelongend,
s.date,
s.starttime,
s.endtime,
s.waterdepthstart,
s.waterdepthend,
g.geartype_geartype,
g.gearname,
s.vesselspeed,
s.distance,
s.trawlvolume,
s.macrosieve,
ts.worrmstaxa_taxonname,
ts.taxaqual_qualifier,
w.validname,
w.rank,
tq.qualifiername,
w.validaphiaid,
ts.abund,
ts.biomass,
su.datapubliclyavailable,
s.samplecode2,
o.ownername,
su.metadata,
su.reference

                  
FROM 
associations.survey as su
INNER JOIN associations.surveysample as ss ON ss.survey_surveyname = su.surveyname 
INNER JOIN samples.sample as s ON ss.sample_samplecode = s.samplecode
INNER JOIN gear.gear as g ON s.gear_gearcode = g.gearcode
INNER JOIN faunal_data.taxasample as ts ON s.samplecode= ts.sample_samplecode 
LEFT JOIN faunal_data.taxaqual as tq ON ts.taxaqual_qualifier = tq.qualifier 
INNER JOIN faunal_data.worrms as w ON w.aphiaid = ts.worrms_aphiaid
INNER JOIN associations.sampleowner as so ON so.sample_samplecode = s.samplecode
INNER JOIN associations.owner as o ON so.owner_ownername = o.ownername
WHERE g.geartype_geartype = 'Trawl'
AND su.surveyname IN ({surveyname*})
ORDER by su.surveyname, s.samplecode,ts.abund desc;",
      surveyname = input$surveyInput,
      .con=pool)
    coord3 <- as.data.frame(dbGetQuery(pool, coord2))
    
    ## Drop the geartype col (only used for querying)
    coord4 <- coord3[,c(1:11,13:30)]
    
    ## Change column names
    colnames(coord4)[1] <- "SurveyName"
    colnames(coord4)[2] <- "SampleCode"
    colnames(coord4)[3] <- "Latitude_Start"
    colnames(coord4)[4] <- "Longitude_Start"
    colnames(coord4)[5] <- "Latitude_End"
    colnames(coord4)[6] <- "Longitude_End"    
    colnames(coord4)[7] <- "Date"
    colnames(coord4)[8] <- "StartTime"    
    colnames(coord4)[9] <- "EndTime"    
    colnames(coord4)[10] <- "WaterDepthStart"  
    colnames(coord4)[11] <- "WaterDepthEnd"  
    colnames(coord4)[12] <- "GearName"
    colnames(coord4)[13] <- "VesselSpeed(knots)"
    colnames(coord4)[14] <- "Distance(m)"
    colnames(coord4)[15] <- "SampleVol(litres)"
    colnames(coord4)[16] <- "MacroSieveSize(mm)"
    colnames(coord4)[17] <- "worrmstaxa_taxonname"
    colnames(coord4)[18] <- "taxaqual_qualifier"
    colnames(coord4)[19] <- "ValidName(WoRMS)"
    colnames(coord4)[20] <- "Rank"
    colnames(coord4)[21] <- "TaxonQualifier"
    colnames(coord4)[22] <- "AphiaID"
    colnames(coord4)[23] <- "Abund"
    colnames(coord4)[24] <- "Biomass"
    colnames(coord4)[25] <- "datapubliclyavailable"
    colnames(coord4)[26] <- "SampleCode2"
    colnames(coord4)[27] <- "DataOwner"
    colnames(coord4)[28] <- "Source"
    colnames(coord4)[29] <- "Reference"
    #colnames(coord4)[18] <- "WaterDepth"
    
    
    return(coord4)
  })
  
  #__________________________________________________________________________________________ 
  #### OUTPUT  METADATA TABLE FOR SELECTED SURVEY(S) ####  
  
  output$coordinates <- DT::renderDataTable({
    coord2 <- unique(coord()[,c(1:4,12,7)])#SurveyName, SampleCode, Latitude, Longitude, GearName, Date  
    DT::datatable(coord2, options = list(pageLength = 9))
    
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT: SAMPLE METADATA AND FAUNAL ABUNDANCES FOR SELECTED SURVEY(S) ####
  
  data2 <- reactive({
    
    #data2 <- coord()[,c(1,2,16,3,4,18,6,5,19,7,10,13,11,12,14,15,17)]
    data2 <- coord()[,c(1:2,26,3:16,19:25,27, 28, 29)]
    #names(data2)
    #1:SurveyName
    #2:SampleCode
    #3:SampleCode2 
    #4:Latitude_Start
    #5:Longitude_Start
    #6:Latitude_End
    #7:Longitude_End
    #8:Date
    #9:StartTime
    #10:EndTime
    #11:WaterDepthStart
    #12:WaterDepthEnd
    #13:GearName   
    #14:VesselSpeed(knots)
    #15:Distance(m)
    #16:SampleVol(litres)
    #17:MacroSieveSize_mm
    #18:ValidName(WoRMS)
    #19:Rank
    #20:TaxonQualifier    
    #21:AphiaID
    #22:Abund
    #23:Biomass
    #24:datapubliclyavailable
    #25:DataOwner 
    #26:Source 
    #27:Reference 

    
    return(data2)
    
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR PUBLICLY AVAILABLE FAUNAL DATA ####
  data4t <- reactive({
    
    data4 <- data2() %>% filter(datapubliclyavailable == TRUE)
    #colnames data4:
    #1:SurveyName
    #2:SampleCode
    #3:SampleCode2 
    #4:Latitude_Start
    #5:Longitude_Start
    #6:Latitude_End
    #7:Longitude_End
    #8:Date
    #9:StartTime
    #10:EndTime
    #11:WaterDepthStart
    #12:WaterDepthEnd
    #13:GearName   
    #14:VesselSpeed(knots)
    #15:Distance(m)
    #16:SampleVol(litres)
    #17:MacroSieveSize_mm
    #18:ValidName(WoRMS)
    #19:Rank
    #20:TaxonQualifier    
    #21:AphiaID
    #22:Abund
    #23:Biomass
    #24:datapubliclyavailable
    #25:DataOwner 
    #26:Source 
    #27:Reference 
    
    data5 <- data4[,c(1:23,25, 26, 27)]  # drop datapubliclyavailable
    
    ## Remove NA from col 14 'TaxonQualifier'
    #data5[14][is.na(data5[14])] <- ""
    
    print(data5)
    
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR NON PUBLICLY AVAILABLE FAUNAL DATA ####
  data4f <- reactive({
    
    data6 <- data2() %>% filter(datapubliclyavailable == FALSE)
    #names(data6)
    #1:SurveyName
    #2:SampleCode
    #3:SampleCode2 
    #4:Latitude_Start
    #5:Longitude_Start
    #6:Latitude_End
    #7:Longitude_End
    #8:Date
    #9:StartTime
    #10:EndTime
    #11:WaterDepthStart
    #12:WaterDepthEnd
    #13:GearName   
    #14:VesselSpeed(knots)
    #15:Distance(m)
    #16:SampleVol(litres)
    #17:MacroSieveSize_mm
    #18:ValidName(WoRMS)
    #19:Rank
    #20:TaxonQualifier    
    #21:AphiaID
    #22:Abund
    #23:Biomass
    #24:datapubliclyavailable
    #25:DataOwner
    #26:Source 
    #27:Reference 
    
    data7 <- unique(data6[,c(1:17,25, 26, 27)]) #i.e. drop faunal data and include only sample metadata
    #names(data7)
    #1:SurveyName
    #2:SampleCode
    #3:SampleCode2 
    #4:Latitude_Start
    #5:Longitude_Start
    #6:Latitude_End
    #7:Longitude_End
    #8:Date
    #9:StartTime
    #10:EndTime
    #11:WaterDepthStart
    #12:WaterDepthEnd
    #13:GearName   
    #14:VesselSpeed(knots)
    #15:Distance(m)
    #16:SampleVol(litres)
    #17:MacroSieveSize_mm
    #18:DataOwner 
    #19:Source
    #20:Reference
    
       print(data7)
    
  })
  
  #__________________________________________________________________________________________
  #### DOWNLOAD FAUNAL DATA FOR SELECTED SURVEY(S) ####  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
      
      # Join 2 reactive objects together. Using bind_rows which assigns "NA" to missing columns
      faunadownload <- dplyr::bind_rows(data4t(), data4f()) 
      #faunadownload <- data4t()##TEST##
      # Replace NAs with 'data witheld'
      #faunadownload[,18:23][is.na(faunadownload[,18:23])] <- "data withheld"     
      
      #####################
      # Replace NAs with 'data witheld' but only for rows where data not publicly available
      pubrows <- nrow(data4t())+1#row number for start of nonpublic data
      faunadownload[pubrows:nrow(faunadownload),18:23][is.na(faunadownload[pubrows:nrow(faunadownload),18:23])] <- "data witheld"
      
      
      
      ##################
      
      write.csv(faunadownload,file,row.names = F)
    })
  
  #__________________________________________________________________________________________  
  
  #### SELECT BY SAMPLE: DEFINE AREA OF INTEREST USING BOUNDING BOX ####
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
  })
  
  ## Bounding box
  bbminlat <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[2]]})# min lat
  bbminlon <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[1]]})# min long
  bbmaxlat <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[2]][[2]]})# max lat
  bbmaxlon <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[1]]})# max long
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT: SAMPLE METADATA FOR SELECTED SAMPLE(S) - SAMPLES WITHIN BOUNDING BOX ####
  
  bbcoord <- reactive({
    
    bbcoord2 <- sqlInterpolate(ANSI(),
                               "SELECT su.surveyname,
s.samplecode,
s.samplelatstart,
s.samplelongstart,
s.samplelatend,
s.samplelongend,
s.date,
s.starttime,
s.endtime,
s.waterdepthstart,
s.waterdepthend,
g.geartype_geartype,
g.gearname,
s.vesselspeed,
s.distance,
s.trawlvolume,
s.macrosieve,
ts.worrmstaxa_taxonname,
ts.taxaqual_qualifier,
w.validname,
w.rank,
tq.qualifiername,
w.validaphiaid,
ts.abund,
ts.biomass,
su.datapubliclyavailable,
s.samplecode2,
o.ownername,
su.metadata,
su.reference
                  
FROM 
associations.survey as su
INNER JOIN associations.surveysample as ss ON ss.survey_surveyname = su.surveyname 
INNER JOIN samples.sample as s ON ss.sample_samplecode = s.samplecode
INNER JOIN gear.gear as g ON s.gear_gearcode = g.gearcode
INNER JOIN faunal_data.taxasample as ts ON s.samplecode= ts.sample_samplecode 
LEFT JOIN faunal_data.taxaqual as tq ON ts.taxaqual_qualifier = tq.qualifier 
INNER JOIN faunal_data.worrms as w ON w.aphiaid = ts.worrms_aphiaid
INNER JOIN associations.sampleowner as so ON so.sample_samplecode = s.samplecode
INNER JOIN associations.owner as o ON so.owner_ownername = o.ownername
WHERE g.geartype_geartype = 'Trawl'
AND s.samplelatstart > ?minlat
AND s.samplelatstart < ?maxlat
AND s.samplelongstart > ?minlong
AND s.samplelongstart < ?maxlong
ORDER by su.surveyname, s.samplecode,ts.abund desc;",
                               minlat = bbminlat(),
                               maxlat = bbmaxlat(),
                               minlong = bbminlon(),
                               maxlong = bbmaxlon())
    
    bbcoord3 <- as.data.frame(dbGetQuery(pool, bbcoord2))
    
    ## Drop the geartype col (only used for querying)
    bbcoord4 <- bbcoord3[,c(1:11,13:30)]
    
    ## Change column names
    #colnames(bbcoord4)[1] <- "SurveyName"
    #colnames(bbcoord4)[2] <- "SampleCode"
    #colnames(bbcoord4)[3] <- "Latitude"
    #colnames(bbcoord4)[4] <- "Longitude"
    #colnames(bbcoord4)[5] <- "GearName"
    #colnames(bbcoord4)[6] <- "Date"
    #colnames(bbcoord4)[7] <- "MacroSieveSize_mm"
    #colnames(bbcoord4)[10] <- "ValidName(WoRMS)"
   # colnames(bbcoord4)[11] <- "Rank"
    #colnames(bbcoord4)[12] <- "TaxonQualifier"
    #colnames(bbcoord4)[13] <- "AphiaID"
    #colnames(bbcoord4)[14] <- "Abund"
    #colnames(bbcoord4)[16] <- "SampleCode2"
    #colnames(bbcoord4)[17] <- "DataOwner"
    #colnames(bbcoord4)[18] <- "WaterDepth"
    #colnames(bbcoord4)[19] <- "SampleSize"
  
    colnames(bbcoord4)[1] <- "SurveyName"
    colnames(bbcoord4)[2] <- "SampleCode"
    colnames(bbcoord4)[3] <- "Latitude_Start"
    colnames(bbcoord4)[4] <- "Longitude_Start"
    colnames(bbcoord4)[5] <- "Latitude_End"
    colnames(bbcoord4)[6] <- "Longitude_End"    
    colnames(bbcoord4)[7] <- "Date"
    colnames(bbcoord4)[8] <- "StartTime"    
    colnames(bbcoord4)[9] <- "EndTime"    
    colnames(bbcoord4)[10] <- "WaterDepthStart"  
    colnames(bbcoord4)[11] <- "WaterDepthEnd"  
    colnames(bbcoord4)[12] <- "GearName"
    colnames(bbcoord4)[13] <- "VesselSpeed(knots)"
    colnames(bbcoord4)[14] <- "Distance(m)"
    colnames(bbcoord4)[15] <- "SampleVol(litres)"
    colnames(bbcoord4)[16] <- "MacroSieveSize(mm)"
    colnames(bbcoord4)[19] <- "ValidName(WoRMS)"
    colnames(bbcoord4)[20] <- "Rank"
    colnames(bbcoord4)[21] <- "TaxonQualifier"
    colnames(bbcoord4)[22] <- "AphiaID"
    colnames(bbcoord4)[23] <- "Abund"
    colnames(bbcoord4)[24] <- "Biomass"
    colnames(bbcoord4)[26] <- "SampleCode2"
    colnames(bbcoord4)[27] <- "DataOwner"
    colnames(bbcoord4)[28] <- "Source"
    colnames(bbcoord4)[29] <- "Reference"  
    
    return(bbcoord4)
  })
  
  #__________________________________________________________________________________________ 
  #### OUTPUT  METADATA TABLE FOR SELECTED SAMPLE(S) - WITHIN BOUNDING BOX ####
  
  output$bbdata <- DT::renderDataTable({
    
    bbdat2 <- unique(bbcoord()[,c(1:4,12,7)])
    #return(bbdat2)
    DT::datatable(bbdat2, options = list(pageLength = 9))
    
  })
  
  #__________________________________________________________________________________________
  #### CREATE A REACTIVE OBJECT FOR PUBLICLY AVAILABLE FAUNAL DATA BY SAMPLE ####
  bbdattrue <- reactive({
    
    bbdatt <- bbcoord() %>% filter(datapubliclyavailable == TRUE)
    #bbdat() columns: 
    #1 "SurveyName"
    #2 "SampleCode"
    #3 "Latitude_Start"
    #4 "Longitude_Start"
    #5 "Latitude_End"
    #6 "Longitude_End"    
    #7 "Date"
    #8 "StartTime"    
    #9 "EndTime"    
    #10 "WaterDepthStart"  
    #11 "WaterDepthEnd"  
    #12 "GearName"
    #13 "VesselSpeed(knots)"
    #14 "Distance(m)"
    #15 "SampleVol(litres)"
    #16 "MacroSieveSize(mm)"
    #17 "worrmstaxa_taxonname"
    #18 "taxaqual_qualifier"
    #19 "ValidName(WoRMS)"
    #20 "Rank"
    #21 "TaxonQualifier"
    #22 "AphiaID"
    #23 "Abund"
    #24 "Biomass"
    #25 "datapubliclyavailable"
    #26 "SampleCode2"
    #27 "DataOwner"
    #28 "Source"
    #29 "Reference"
    
    ## Drop unwanted cols: 17 worrmstaxa_taxonname, 18 taxaqual_qualifier, 25 datapubliclyavailable
    all <- bbdatt[,c(1:2,26,3:16,19:24,27,28,29)]
    #all colnames:
    #1 "SurveyName"
    #2 "SampleCode"
    #3 "SampleCode2"
    #4 "Latitude_Start"
    #5 "Longitude_Start"
    #6 "Latitude_End"
    #7 "Longitude_End"    
    #8"Date"
    #9 "StartTime"    
    #10 "EndTime"    
    #11 "WaterDepthStart"  
    #12 "WaterDepthEnd"  
    #13 "GearName"
    #14 "VesselSpeed(knots)"
    #15 "Distance(m)"
    #16 "SampleVol(litres)"
    #17 "MacroSieveSize(mm)"
    #18 "ValidName(WoRMS)"
    #19 "Rank"
    #20 "TaxonQualifier"
    #21 "AphiaID"
    #22 "Abund"
    #23 "Biomass"
    #24 "DataOwner"
    #25 "Source"
    #26 "Reference"
    print(all)
    
  })
  
  #__________________________________________________________________________________________ 
  #### CREATE A REACTIVE OBJECT FOR NON PUBLICLY AVAILABLE FAUNAL DATA BY SAMPLE ####
  
  bbdatfalse <- reactive({
    
    bbdatf <- bbcoord() %>% filter(datapubliclyavailable == FALSE)
    
    #bbdatf() columns: 
    #1 "SurveyName"
    #2 "SampleCode"
    #3 "Latitude_Start"
    #4 "Longitude_Start"
    #5 "Latitude_End"
    #6 "Longitude_End"    
    #7 "Date"
    #8 "StartTime"    
    #9 "EndTime"    
    #10 "WaterDepthStart"  
    #11 "WaterDepthEnd"  
    #12 "GearName"
    #13 "VesselSpeed(knots)"
    #14 "Distance(m)"
    #15 "SampleVol(litres)"
    #16 "MacroSieveSize(mm)"
    #17 "worrmstaxa_taxonname"
    #18 "taxaqual_qualifier"
    #19 "ValidName(WoRMS)"
    #20 "Rank"
    #21 "TaxonQualifier"
    #22 "AphiaID"
    #23 "Abund"
    #24 "Biomass"
    #25 "datapubliclyavailable"
    #26 "SampleCode2"
    #27 "DataOwner"
    #28 "Source"
    #29 "Reference"
    
    ## Take sample cols as for the public data (minus the faunal info (cols 19:24))
    bbdatf2 <- unique(bbdatf[,c(1:2,26,3:16,27,28,29)])#19:24,
    #bbdatf2 columns: 
    #1 "SurveyName"
    #2 "SampleCode"
    #3 "SampleCode2"
    #4 "Latitude_Start"
    #5 "Longitude_Start"
    #6 "Latitude_End"
    #7 "Longitude_End"    
    #8 "Date"
    #9 "StartTime"    
    #10 "EndTime"    
    #11 "WaterDepthStart"  
    #12 "WaterDepthEnd"  
    #13 "GearName"
    #14 "VesselSpeed(knots)"
    #15 "Distance(m)"
    #16 "SampleVol(litres)"
    #17 "MacroSieveSize(mm)"
    #18 "DataOwner"
    

    print(bbdatf2)
    
  })
  
  #__________________________________________________________________________________________
  #### DOWNLOAD FAUNAL DATA FOR SELECTED SAMPLE(S) #### 
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")
    },
    content = function(file) {
      
      # Join 2 reactive objects together. Using bind_rows which assigns "NA" to missing columns
      faunadownload2 <- dplyr::bind_rows(bbdattrue(), bbdatfalse())

      
      # Replace NAs with 'data witheld' but only for rows where data not publicly available
      pubrows2 <- nrow(bbdattrue())+1#row number for start of nonpublic data
      faunadownload2[pubrows2:nrow(faunadownload2),18:23][is.na(faunadownload2[pubrows2:nrow(faunadownload2),18:23])] <- "data witheld"
      
      
      write.csv(faunadownload2,file,row.names = F)
    })
  
  #__________________________________________________________________________________________

}
shinyApp(ui = ui, server = server)