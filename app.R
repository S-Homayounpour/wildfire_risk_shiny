#There is a problem in using the sqllite function inside a reactive expression
#So I want to investigate if there is a better way to calculate the 
# risk without using database.
# Up to now I noticed that there is a problem with sample_raster function as it turns NA
# my observation is that extract function inside the sample_raster
# function does not work when layeers are reactive objects
# as it returns NA value for value of rasters at that point
#I am using helper function which is the main body of sample_raster function to find the value why
#extarct is returning NA Values
#My guess is that there is a problem with crs , there is mismatch between crs of tsf and crs of lines
#The problem is that loc_point is in 4326 while the tsf is not and lines genereted is not
# so the workaround is that to convert all crs to 4326
# another thing that I noticed is that st_line_sample function inside helper function works so weired
# it is adding generating sample points in lines with large numbers in xy coordinates
# while st_line_sample works fine with  lines variable in leaflet_test script
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(raster)
library(rgdal)
library(here)
library(tidyverse)
library(ggplot2)
library(WildfireRisk)
library(RSQLite)
library(DT)
source("treat_blocks_lines_output.R",local = TRUE)
#global variable
### These are the required objects for running the shint  
tsf <- raster( here("test_leaflet/data/act_rasters/actplus_tsf17/hdr.adf") )

locations <- read.csv( here("test_leaflet/data/ACT_Test_Data/act_cens11_centres.CSV") )

forest <- raster( here("test_leaflet/data/act_rasters/actfor_comb/hdr.adf") )

burnblocks <- st_read(here("test_leaflet/data/ACT_Test_Data/ACTBurnBlocks_gt1.shp"))

locations[, 1] <- stringr::str_trim( sprintf("%12.0f", locations[, 1]) )

ignitionprob <- raster(here("test_leaflet/data/act_rasters/ignition/act_igden_n2/hdr.adf"))

ign_vals <- raster::extract(ignitionprob,locations[,c(2,3)],method = "bilinear")

ign_vals_df <- cbind(locations,ign_vals)

ign_vals_df <- st_as_sf(ign_vals_df,coords = c("x","y"),crs = 28355)

ign_vals_df <- st_transform(ign_vals_df,crs = 4326)

initial_risks_loc <- st_read(here("initial_risk.shp")) 

initial_risks_loc <- st_transform(initial_risks_loc,crs = 4326)

loc_coords <- as.data.frame(st_coordinates(initial_risks_loc))

loc_coords$id <-  1:nrow(initial_risks_loc)

loc_coords$pobs_mean <-  initial_risks_loc$pobs_mean




# UI design for dashboard
ui <- navbarPage(title = "Wildfire risk",id = "inTabset",theme = shinytheme("superhero"),
   # Sidebar with a numeric input for selecting number of generated lines
   # and action buttons for stopping sampling and generating lines
   # stop_selecting lets other reactives know that point selection is stopped
   # clicking on next_step_0 will jump from select point tab to the investigate the generated lines tab
   # clicking on restart button set the reactiveValues of the selected point to zero
         tabPanel(title = "1. Select Point", value = "select_points" ,
          fluidRow(
            column(4,wellPanel(
            h1("Instruction"),
            p("1. Click on census blocks (coloured points) to choose your desired locations."),
            tags$p("2. Once you have selected the points click on the ",tags$b("'Point selection is stopped'"),"button."),
            tags$p("3. If youn are happy with your selected points click on the ",tags$b("'Go to the next step'"),"button.
                    Otherwise click on the ",tags$b("'Restart point selection'"),"to restart the point selection process")
          ),
          wellPanel(
            h2("Note"),
            tags$p("This dashboard is designed based on the: ",tags$br(),
                  tags$strong(tags$q("Price Owen, Borah Rittick, Bradstock Ross, 
                   Penman Trent (2015) An empirical wildfire 
                  risk analysis: the probability of a fire 
                  spreading to the urban interface in Sydney, 
                  Australia. International Journal of Wildland 
                  Fire 24, 597-606")))
          )
          ),        
          column(8,
          wellPanel(
          numericInput(inputId =  "nlines",label = "Number of lines to generate",min = 1, max = 100,value=80), 
          actionButton("stop_selecting","Point selection is stopped",class = "btn-info"),
          actionButton("next_step_0","Go to the next step",class = "btn-warning"),
          actionButton("restart","Restart point selection",class = "btn-success"),
          leafletOutput("raw_map",width = "100%",height = 700)
          )))
                  ),
          
   # 'Investigate the generated lines' tab will show the generated lines that has generated
   # in the selected points
   # show_lines button should be clicked till generated lines plot pops up
   # next_step_1 button should be clicked to go to the next step 
         tabPanel( title = "2. Investigate the generated lines",value = "gen_lines",
          fluidRow(
            column(4,
            wellPanel(
              h2("Instruction"),
              tags$p("1. Click on ",tags$b("'Generate lines'"),
                     " button to probe risk of fore spreading across each sample line ."),
              tags$p("2. If you are happy with the result click on the",tags$b("'Go to the next step'"),"button.")
            )              
                          ),
         column(8,
                wellPanel(
                  actionButton("show_lines","Generate Lines",class = "btn-info"),
         actionButton("next_step_1","Go to the next step",class = "btn-warning"),
         leafletOutput("gen_lines_map",width = "100%",height = 700)
         ))
          )),
         tabPanel( title =  "3. Ignition distribution across study site", value = "ignition_dist",
                       fluidRow(column(4,
                         wellPanel(
                           h2("Instruction"),
                           tags$p("1. click on the ",tags$b("'Show ignition distribution'"),
                                  " button to probe the ignition distribution across the region"),
                           tags$p("2. If you are happy with the result click on the ",
                                  tags$b("'Go to next step'")," button.")
                           )) ,
                       column(8,
                        wellPanel(
                       actionButton("show_dist","Show ignition distribution",class = "btn-info"),
                       actionButton("next_step_2","Go to the next step",class = "btn-warning"),
                       leafletOutput("ignition_dist",width = "100%",height = 700)
                       ))
                       ))
          ,
   # 'Treatment blocks exposure' include plot that shows the ignition probability and fire risk
   # the 'show_ignition' button should be clicked to view the fire risk and treatment block exposure  
   tabPanel(title = "4. Treatment blocks exposure",value = "treat_blocks",
            fluidRow(
            column(4,
              wellPanel(
              h2("Instruction"),
              tags$p("1. Click on the ",
                     tags$b("'Show fire risk in selected points'"), " button to probe what 
                     is the probability of fire getting to those points."),
              tags$p("2. You can click on the ",tags$b("'Go to next step'"),
                     " button to treat blocks and evaluate risk reduction in each point")
              )),  
            column(8,
            wellPanel(
            actionButton("show_ignition","Show fire risk in selected points",class = "btn-warning"),
            actionButton("next_step_3","Go to the next step",class = "btn-info"),
            leafletOutput("ignition_map",width = "100%",height = 700)
             ))
             )),
  
   tabPanel( title = "5. Treating blocks and assessing fire receiving probability", value = "risk_assessment",
             fluidRow(
             column(4,wellPanel(
              h2("Instruction"),
              tags$p("1. Click on the polygons to select the treatment blocks"),
              tags$p("2. Once you have selected the treatment blocks click on the",
                     tags$b("'Polygon selection is stopped'")),
              tags$p("3. If you are happy with selected polygons click on",
                     tags$b("'Assess risk'"),". Otherwise you can click on ",
                     tags$b("'Restart the polygon selection'")," to choose another set of treatment blocks. ")
             )), 
             column(8,
             wellPanel(
             actionButton("stop_selecting_poliges","Polygon selection is stopped",class = "btn-warning"),
             actionButton("restart_polygons","Restart the polygon selection",class = "btn-info"),
             actionButton("assess_risk","Assess risk",class = "btn-danger"),
             leafletOutput("risk_assess_map",width = "100%",height = 700))
             )),
             
             fluidRow(
               column(12,
                 wellPanel(
                 style = "color: black;",
                 dataTableOutput("reduced_risk_tab")))
               ))
            
            )
   
          

                   
   
# Define server logic required to draw a histogram
server <- function(input, output,session) {
##how to capture the clicks stream
## reactiveValues of Dclckd is used to capture the click stream
## reactiveValues of selected_longlat is used to keep the captured data valid when
## stop_selecting button is selected which sets Dclckd to NULL
  

Dclckd <- reactiveValues(lng = 0, lat = 0,id = 0)

selected_longlat <- reactiveValues(lng = 0,lat= 0,id = 0)

pol_clicked <- reactiveValues(id = 0)

pol_selected <- reactiveValues(id = 0)

### Taking output of stream click
## Dclckd is for caturing the click stream
## selected_longlat is a temp variable 
## for storing the click stream
observeEvent(input$raw_map_marker_click,{
  
  Dclckd$lng <- c(Dclckd$lng,input$raw_map_marker_click$lng)
  
  Dclckd$lat <- c(Dclckd$lat,input$raw_map_marker_click$lat)
  
  Dclckd$id <- c(Dclckd$id,input$raw_map_marker_click$id)
  
  selected_longlat$lng <- Dclckd$lng
  
  selected_longlat$lat <- Dclckd$lat
  
  selected_longlat$id <- Dclckd$id
  
})
## Putting selected points into a reactive object
## for future use in leaflet map
## for example highlighting the selected centroids 
## or generating lines
loc_points <- eventReactive( input$stop_selecting,{
  
  distinct(tibble(lng = selected_longlat$lng,lat = selected_longlat$lat),lng,lat,.keep_all = TRUE)
  
})

## Capturing the ids of selected points
## it is assumed that it can be helpful 
## for future use 
## Not sure when?
loc_points_id <- eventReactive(input$stop_selecting,{
         unique(selected_longlat$id)
})

observe(
  print(loc_points_id())
)
# setting reactiveValues of Dclckd to null
# when the stop_selecting button is selected 

observeEvent(input$stop_selecting,{
  Dclckd$lng <- 0
  Dclckd$lat <- 0
  Dclckd$id <- 0
})

## Set selected_point to null 
## when restart button is clicked
## there should be a restart button
## and a stop_selecting button 
## as restart button set the selected points to null
observeEvent(input$restart,{
  
  selected_longlat$lng <- 0
  selected_longlat$lat <- 0
  selected_longlat$id <- 0
})

## generates lines coordinates when  stop_selecting is clicked
length_fun <- function(n) 100 + rexp_truncated(n, 1/5000, 20000 - 100)
lines <- eventReactive(input$stop_selecting,{
  lcp_nocrs <- st_as_sf(isolate(loc_points()[-1,]),coords = c("lng","lat"),crs = 4326)
  lcp_nocrs <- st_transform(lcp_nocrs,crs = 28355)
   make_scan_lines(st_coordinates(lcp_nocrs), input$nlines, lengths = length_fun,crs = tsf)

})

## genereate lines and assign risks to the lines
# risk.lines <- eventReactive(input$show_lines,{
#   calculate_risk(lines(), tsf, forest, sample.spacing = 100)
# 
# })

risk.lines <- reactive({
  calculate_risk(lines(), tsf, forest, sample.spacing = 100)
})

## calculate the risk of recieving fire to selected points
## based on the generated lines
risk.loc <- reactive({
    dat <- summarize_location_risk(risk.lines())
    st_transform(dat,crs = 4326)
  })

## find the number of lines intersecting with the 
## with each block
## and its probability
## it will be useful when  
## we want to plot the treatment blocks effect
risk.block <- reactive({
    
    dat <- summarize_block_risk(risk.lines(), burnblocks) 
    dat <- st_transform(dat,crs = 4326) %>% mutate(id = 1:nrow(.))
    dat

  })



## the chance of ignition for each selected location 
ignition.loc <- reactive({
  dat <- summarize_location_nbrhood(lines(), ignitionprob)
         st_transform(dat,crs = 4326)
})

#getting Ids of selected polygons 
## selected polygons are treatment block in fact
## we have again two variables for storing the click streams
## pol_clicked is the first one to capture the click stream
## pol_selected is the temp variable for storing the click stream
## it is helpful to have two variables
## the first is set to zero when the stop_ button is clicked
## the second one is set to zero when the restart button is clicked
observeEvent(input$risk_assess_map_shape_click,{
  pol_clicked$id <- c(pol_clicked$id,input$risk_assess_map_shape_click$id)
  print(pol_clicked$id)
  pol_selected$id <- pol_clicked$id
  print(pol_selected$id)
})

## putting id of clicked polygon in a reactive function 
selected_treatement_blocks <- eventReactive(input$stop_selecting_poliges,{
  unique(pol_selected$id)
})

#restart pol_clicked if selecting polygon is stopped
observeEvent(input$stop_selecting_poliges,{
  pol_clicked$id <- 0
})
# set the second one to zero when the restart button is clicked
observeEvent(input$restart_polygons,{
  pol_selected$id <- 0
})

# when the stop selecting the polygons button 
# is clicked then print the selected treated blocks ids
observeEvent(input$stop_selecting_poliges,{
print(paste(" selected blocks are",selected_treatement_blocks()))
})

## returning the geometries of the selected 
## treatment blocks and setting their coordinates to
## zone 55 which its code is 28355
## we change the coordinates to 28355 because 
## we want to use it in treated blocks
treatedblocks <- reactive({
 a <-  risk.block()[risk.block()$id %in% selected_treatement_blocks(), ]
 st_transform(a,crs = 28355)
})

## calculating risk lines after treating blocks
treated_risk_line <- reactive({
  a <- do_treat_blocks_line(treatedblocks(),risk.lines())
  a
  })

## summary of risk for each locations after treatment
loc_risk_summ_treated <- eventReactive(input$assess_risk,{
  summarize_location_risk_noDB(treated_risk_line())
  
})


## capturing IDs of treatment block and decrease in risk of location after treatement
##paste(selected_treatement_blocks(),sep = " "),
risk_reduced_loc <-  reactiveValues(rws = list())
ids_treated_blocks <-  reactiveValues(rws = list())


observeEvent( input$assess_risk,{
  temp1 <- list(loc_risk_summ_treated()$pobs_mean - risk.loc()$pobs_mean)
  risk_reduced_loc$rws <- c(risk_reduced_loc$rws,temp1)
  print( risk_reduced_loc$rws)
  
  temp2 <- paste(selected_treatement_blocks()[-1],collapse = ",")
  ids_treated_blocks$rws <- c(ids_treated_blocks$rws,temp2) 
  print(ids_treated_blocks$rws)
    })

#set selected polygons in datatable to zero when new set of points are selected
observeEvent(input$restart,{
  risk_reduced_loc$rws <- list()
  ids_treated_blocks$rws <- list()
})



nms <- reactive({
  numb_of_locs <- length(loc_points_id()[-1])
  ind_loc <- 1:numb_of_locs
  locs <- as.vector(unlist(sapply("location",paste,ind_loc,sep = " ")))
  ns <- c("ID","treated_blocks",locs)
  ns
})

x <- reactive({
  numb_of_locs <- length(loc_points_id()[-1])
  res <- data.frame(matrix(ncol = 1+numb_of_locs),stringAsFactors = FALSE)
  temp1 <-  do.call(rbind.data.frame,risk_reduced_loc$rws)
  temp2 <- do.call(rbind.data.frame,ids_treated_blocks$rws)
  res <- cbind.data.frame(temp2,temp1)
  res <- unname(res)
  res
  })



observe({
  print("selected pols ")
  print(x())
  str(nms())
  str(x())
})



pal_points <- reactive({
  palette_rev <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))
  
  colorNumeric(
    palette = palette_rev,
    domain = loc_coords$pobs_mean)
})  


output$raw_map <- renderLeaflet({
      # display leaflet map
pal <-  pal_points()
leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  20,doubleClickZoom= FALSE)) %>% 
addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
setView(149,-35.5,zoom = 9) %>% 
setMaxBounds(lng1=148,
                    lat1 =-34.5,
                    lng2 = 150,
                    lat2 = -37) %>% 
addCircleMarkers(data = loc_coords,lng=~X,lat = ~Y,radius = 1, fillColor = ~pal(pobs_mean),
                        color = ~pal(pobs_mean), fillOpacity =  0.8,opacity = 0.8,layerId = ~id,
                        popup = paste(tags$b("Census coordinates"),tags$br(),tags$hr(),
                                      tags$em("Long : "),"<b> ",round(loc_coords$X,4),"</b>",tags$br(),
                                      tags$em("Lat : "),"<b> ",round(loc_coords$Y,4)," </b>", tags$br(),
                                      tags$em("id :"),"<b>",loc_coords$id,"</b>") ) %>% 
addLegend(data = loc_coords,"topright", pal = pal,
            values = ~pobs_mean,title = "Initial risk",opacity = 0.8 )
     
})

## treating blocks and assessing risk

observeEvent(input$stop_selecting,{
  leafletProxy("raw_map") %>%
    clearShapes() %>%
  addCircleMarkers(data = loc_coords[loc_points_id(),],lng = ~X,lat = ~Y,
                   color = "#e41a1c",fillColor = "#b3cde3", 
                   opacity = 1,fillOpacity = 0.8,weight = 3.5
                   ,group = "selected") 
})

observeEvent(input$restart,{
leafletProxy("raw_map") %>% 
    clearGroup("selected")
})

pal_lines <- reactive({
  palette_rev <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))
  
  colorNumeric(
    palette = palette_rev,
    domain = risk.lines_sf()$pobs)
})  

risk.lines_sf <- reactive({
  
  dat <-  st_as_sf(risk.lines(),crs = 28355)
  st_transform(dat,crs =  4326)
})

observeEvent(input$next_step_0,{
  updateTabsetPanel(session, "inTabset",
                    selected = "gen_lines")
})

##making a dependecy between line plot and generate lines button
points_clicked_by <- eventReactive(input$show_lines,{loc_points()})
lines_clicked_by <- eventReactive(input$show_lines,{risk.lines_sf()})

observeEvent(input$show_lines,{
  output$gen_lines_map <- renderLeaflet({
  pal <- pal_lines()
  leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  20,doubleClickZoom= FALSE)) %>% 
    addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
    setView(149,-35.5,zoom = 9) %>% 
    setMaxBounds(lng1=148,
                 lat1 =-34.5,
                 lng2 = 150,
                 lat2 = -37) %>% 
  addCircleMarkers(data = points_clicked_by(),radius = 1, fillColor = "red",color = "red", fillOpacity =  0.4) %>%
  addPolylines(data = lines_clicked_by(),fillColor = ~pal(pobs),
                 color = ~pal(pobs),fillOpacity = 1,weight = 2) %>%
    addLegend(data = lines_clicked_by(),"bottomright", pal = pal,
              values = ~pobs,title = "Fire probability",opacity = 1 )

})
}) 


observeEvent(input$next_step_2,{
  
  updateTabsetPanel(session, "inTabset",
                    selected = "treat_blocks")
})


## making a dependency to plot risk block data to show ignition 
risk_loc_clicked_by <-  eventReactive(input$show_ignition,{
risk.loc()  
})

risk_block_clicked_by <-  eventReactive(input$show_ignition,{
  risk.block()
})
pal_lines_2 <- reactive({ 
  palette_rev <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))
  pal <- colorNumeric(
    palette = palette_rev,
    domain = risk_loc_clicked_by()$pobs_mean)
})

intrsc_col <- reactive({
  colorNumeric(palette = "RdPu",domain = risk_block_clicked_by()$nlines)
})

observeEvent(input$show_ignition,{

 output$ignition_map <- renderLeaflet({
   intrsc_col <- intrsc_col()
   pal_1 <- pal_lines_2()
   
    leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  20,doubleClickZoom= FALSE)) %>% 
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
      setView(149,-35.5,zoom = 9) %>% 
      setMaxBounds(lng1=148,
                   lat1 =-34.5,
                   lng2 = 150,
                   lat2 = -37) %>% 
    addPolygons(data = risk_block_clicked_by(),color = "#3182bd",stroke = TRUE,
                fillColor = ~intrsc_col(nlines),opacity =0.5,smoothFactor = 0.1,
                fillOpacity = 1, weight = 1) %>%
    addLegend(data = risk_block_clicked_by(),"bottomright", pal = intrsc_col, values = ~nlines,
              title = "Number of intersected lines",
              opacity = 1 ) %>%
    addCircles(data = risk_loc_clicked_by(),color = ~pal_1(pobs_mean),fillColor = ~pal_1(pobs_mean),
               radius = 100,
               fillOpacity = 1,opacity = 1,
               label = ~as.character(paste("Expected probability of",
                                           paste(locationid,"th census block receiving fire is",sep=""),
                                           round(pobs_mean,4)))) %>%
    addLegend(data = risk_loc_clicked_by(),"topleft", pal = pal_1, values = ~pobs_mean,
              title = "probability",opacity = 1) 
  
})
})
 

observeEvent(input$next_step_1,{

  updateTabsetPanel(session, "inTabset",
                    selected = "ignition_dist")
  })

ignition_loc_clicked_by <- eventReactive(input$show_dist,{
  ignition.loc()
})
  
getcols <- reactive({
  
  dat <- sapply(ignition_loc_clicked_by()$layer_mean, function(layer_mean){
    
    if(layer_mean <= 0.2) {
      "green"
    } else if(layer_mean <= 0.3) {
      "orange"
    } else {
      "red"
    }
  })
  dat
})

icons <- reactive({
  
  awesomeIcons(
    icon = 'flame',
    iconColor = 'black',
    library = 'ion',
    markerColor = getcols()
  )
})

observeEvent(input$show_dist,{
 
  output$ignition_dist <- renderLeaflet({
    icons <- icons()
    leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  20,doubleClickZoom= FALSE)) %>% 
                                          addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
                                          setView(149,-35.5,zoom = 9) %>% 
                                          setMaxBounds(lng1=148,
                                                       lat1 =-34.5,
                                                       lng2 = 150,
                                                       lat2 = -37) %>% 
    addHeatmap(data = ign_vals_df,intensity = ~ign_vals,
               blur = 10, max = 0.2, radius = 10 ) %>% 
    addAwesomeMarkers(data = ignition_loc_clicked_by(), icon = icons,
                      label = ~as.character(paste("Mean probability of ignition in this grid cell is ",
                                            round(layer_mean,4))))
})
})



observeEvent(input$next_step_3,{
  updateTabsetPanel(session, "inTabset",
                    selected = "risk_assessment")
  
  intrsc_col <- intrsc_col()
  
  pal_1 <- pal_lines_2()
  
  icons <- icons()
  
  output$risk_assess_map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  20,doubleClickZoom= FALSE)) %>% 
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
      setView(149,-35.5,zoom = 9) %>% 
      setMaxBounds(lng1=148,
                   lat1 =-34.5,
                   lng2 = 150,
                   lat2 = -37) %>% 
      addPolygons(data = risk_block_clicked_by(),color = "#3182bd",stroke = TRUE,
                  fillColor = ~intrsc_col(nlines),opacity =0.5,smoothFactor = 0.1,
                  fillOpacity = 1, weight = 1,layerId = ~id,
                  label = ~as.character(id),
                  labelOptions = labelOptions(direction = 'top')) %>%
      addLegend(data = risk_block_clicked_by(),"bottomright", pal = intrsc_col, values = ~nlines,
                title = "Number of intersected lines",
                opacity = 1 ) %>%
      addCircles(data = risk_loc_clicked_by(),color = ~pal_1(pobs_mean),fillColor = ~pal_1(pobs_mean),
                 radius = 100,
                 fillOpacity = 1,opacity = 1,
                 label = ~as.character(paste("Expected probability of",
                                             paste(locationid,"th census block receiving fire is",sep=""),
                                             round(pobs_mean,4))))
})
})

observeEvent(input$stop_selecting_poliges,{
  leafletProxy("risk_assess_map") %>% 
    addPolygons(data = st_transform(treatedblocks(),crs = 4326),fillColor = "#7c2404",stroke = TRUE,weight = 1.5,
                color ="#e82c06",opacity = 1,fillOpacity = 1,group = "added")
})

observeEvent(input$restart_polygons,{
  leafletProxy("risk_assess_map") %>% 
    clearGroup("added")
})

# dt <- reactive({
#   d <- datatable(x(),
#                  colnames = nms()) 
# })


# observe(
#   
#   print(dt())
# )

output$reduced_risk_tab <- DT::renderDataTable({
  DT::datatable(x(),
            colnames = nms()) 
})

}
# Run the application 
shinyApp(ui = ui, server = server)

