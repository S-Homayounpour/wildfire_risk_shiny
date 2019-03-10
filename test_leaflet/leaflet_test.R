library(shiny)
library(leaflet)
library(sf)
library(raster)
library(rgdal)
library(here)
library(tidyverse)
library(ggplot2)
library(WildfireRisk)
library(RSQLite)
library(leaflet.extras)
library(ggplot2)
library(ggmap)
source("./treat_blocks_lines_output.R")
## reading the location of centroids of census blocks
##-35.920519, 149.046376 down south
## -35.124760, 149.121996 up north
## -35.319155, 149.399271 east
## -35.649132, 148.769161 west
tsf <- raster( here("test_leaflet/data/act_rasters/actplus_tsf17/hdr.adf") )

forest <- raster( here("test_leaflet/data/act_rasters/actfor_comb/hdr.adf") )

burnblocks <- st_read(here("test_leaflet/data/ACT_Test_Data/ACTBurnBlocks_gt1.shp"))

ignitionprob <- raster(here("test_leaflet/data/act_rasters/ignition/act_igden_n2/hdr.adf"))

meshnlocks <- st_read(here("test_leaflet/data/ACT_meshBlocks/MB_2016_ACT.shp"))


locations <- read.csv( here("test_leaflet/data/ACT_Test_Data/act_cens11_centres.CSV") )

locations[, 1] <- stringr::str_trim( sprintf("%12.0f", locations[, 1]) )



######
locations_sf <- st_as_sf(locations[,c(2,3)],coords = c("x","y"),crs = 28355)

locations_sf <- st_transform(locations_sf,crs = 4326)
loc_coords <- as.data.frame(st_coordinates(locations_sf))


leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  20)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addCircleMarkers(data = locations_sf,radius = 0.5, fillColor = "red",
                   color = "red", fillOpacity =  0.5,opacity = 0.5,
                   popup = paste(tags$h4("Point Coords"),
                    tags$strong("Long:"),round(loc_coords$X,5),"<br>",
                    tags$strong("Lat:"),round(loc_coords$Y,5)) 
            ) %>% 
      addPolygons(data = meshnlocks,label = ~SA2_NAME16,weight = 1,fillOpacity = 0,color = "black")


########

range(locations$x)
range(locations$y)

ii <- sample(1:nrow(locations), 10)
dat <- locations[ii, ]

length_fun <- function(n) 100 + rexp_truncated(n, 1/5000, 20000 - 100)
lines <- make_scan_lines(dat, 180, lengths = length_fun,crs = tsf)
ignition.loc <- summarize_location_nbrhood(lines, ignitionprob)

######
risk.lines <- calculate_risk(lines, tsf, forest, sample.spacing = 100)

risk.loc <- summarize_location_risk(risk.lines)
risk.loc  
attr(risk.lines,"dbname")

risk.block <- summarize_block_risk(risk.lines, burnblocks)
risk.block

ind <- sample.int(861,160) 
treateblocks <- risk.block[ind,]




enough.mem <- pointdata_memory(risk.lines, "Gb") < 2
risk.treat.blocks <- treat_blocks(risk.block,risk.lines,in.memory = enough.mem)
risk.treat.blocks




risk.selected.blocks <- treat_blocks(treateblocks,risk.lines)
risk.selected.blocks
####


locations <- st_as_sf(locations[,c(2,3)],coords = c("x","y"),crs = 28355)

locations<- st_transform(locations,crs = 4326)
lines <-  st_transform(lines , crs= 4326)
burnblocks <-  st_transform(burnblocks , crs= 4326)

loc_coords <- as.data.frame(st_coordinates(locations))
# step1 choosing centroids
#######
leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addCircleMarkers(data = loc_coords,lng = ~X,lat = ~Y,radius = 1, fillColor = "red",color = "red", fillOpacity =  0.4,popup = ~c(as.character(X),as.character(Y)))


##step 2 adding generated lines to the dashboard
## adding generated line to leaflet
dat_sf <- st_as_sf(dat[c(2,3)],coords = c("x","y"),crs = 28355)
dat_sf <-  st_transform(dat_sf,crs = 4326)

leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addCircleMarkers(data = loc_coords,lng = ~X,lat = ~Y,radius = 1, fillColor = "red",color = "red", fillOpacity =  0.4,popup = ~c(as.character(X),as.character(Y))) %>% 
  addPolylines(data = lines,fillColor = "black",color = "black",fillOpacity = 1,weight = 2) 


###assigning a colour to the line

risk_lines_sf <- st_transform(risk.lines,crs = 4326)


palette_rev <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))

pal_lines <- colorNumeric(
  palette = palette_rev,
  domain = risk_lines_sf$pobs)

leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addCircleMarkers(data = dat_sf,radius = 1, fillColor = "red",color = "red", fillOpacity =  0.4) %>% 
  addPolylines(data = risk_lines_sf,fillColor = ~pal_lines(pobs),
               color = ~pal_lines(pobs),fillOpacity = 1,weight = 2) %>% 
addLegend(data = risk_lines_sf,"bottomright", pal = pal_lines, 
          values = ~pobs,title = "Fire probability",opacity = 1 ) 


##converting to the sf object
######
risk_loc_sf <- st_transform(risk.loc,crs = 28355)
risk_loc_sf <- st_transform(risk.loc,crs = 4326 )

igninition_loc_sf <- st_transform(ignition.loc,crs = 28355)
igninition_loc_sf <- st_transform(ignition.loc,crs = 4326)

### adding the ignition probability and risk of receiving the fire to the package 
#######testing the fire legend
###########
IconSet <- awesomeIconList(
  "high chance of ignition"   = makeAwesomeIcon(icon= 'fire', markerColor = 'red', iconColor = 'black', library = "fa"),
  "moderate chance of ignition" = makeAwesomeIcon(icon= 'fire', markerColor = 'orange', iconColor = 'black', library = "fa"),
  "low chance of ignition" = makeAwesomeIcon(icon= 'fire', markerColor = 'green', iconColor = 'black', library = "fa")
  )
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 8px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: -20px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}









######
palette_rev <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))
pal <- colorNumeric(
  palette = palette_rev,
  domain = risk_loc_sf$pobs_mean)

getcols <- colorNumeric(palette ="OrRd" ,domain = igninition_loc_sf$layer_mean)

icons <- awesomeIcons(
  icon = 'flame',
  iconColor = 'black',
  library = 'ion',
  markerColor = getcols(igninition_loc_sf$layer_mean)
)

leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addCircles(data = risk_loc_sf,color = ~pal(pobs_mean),fillColor = ~pal(pobs_mean),
             radius = 1400,
             fillOpacity = 1,opacity = 1,label = ~as.character(paste("Expected probability of this census receiving fire is",round(pobs_mean,4)))) %>%
  addLegend(data = risk_loc_sf,"bottomright", pal = pal, values = ~pobs_mean,
            title = "Fire probability",
            opacity = 1 ) %>% 
  addAwesomeMarkers(data = igninition_loc_sf, icon = icons,
                    label = ~as.character(paste("Mean probability of ignition in this grid cell is ",round(layer_mean,4)))) %>% 
addControl(html = markerLegendHTML(IconSet = IconSet), position = "topleft")
  


#### Next step visualising the intersection of lines and burnblocks 
risk_block_sf <- st_transform(risk.block,crs = 28355)
risk_block_sf <- st_transform(risk.block,crs = 4326)


### Treatment block or burn block visualisation
#########
### adding the treatment blocks to the plot
intrsc_col <- colorNumeric(palette = "YlOrRd",domain = risk_block_sf$nlines)

leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addPolygons(data = risk_block_sf,color = ~intrsc_col(nlines),
              fillColor = ~intrsc_col(nlines),opacity =1,fillOpacity = 1) %>% 
  addLegend(data = risk_block_sf,"bottomright", pal = intrsc_col, values = ~nlines,
            title = "Intersection probability",
            opacity = 1 ) %>% 
addCircles(data = risk_loc_sf,color = ~pal(pobs_mean),fillColor = ~pal(pobs_mean),
           radius = 400,
           fillOpacity = 1,opacity = 1,label = ~as.character(paste("Expected probability of this census receiving fire is",round(pobs_mean,4)))) %>%
  addLegend(data = risk_loc_sf,"bottomright", pal = pal, values = ~pobs_mean,
            title = "Fire probability",
            opacity = 1 ) %>% 
  addAwesomeMarkers(data = igninition_loc_sf, icon = icons,
                    label = ~as.character(paste("Mean probability of ignition in this grid cell is ",round(layer_mean,4))))



######
tsf_agg <- aggregate(tsf,fun = mean,fact = 10)
sr <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
tsf_agg_projected <- projectRaster(tsf_agg, crs = sr)


### How to show the density of the time since fire 

ggplot(as_tibble(getValues(tsf_agg))) + 
  geom_density(aes(x = value)) 

## showing ignition points









ign_agg <- aggregate(ignitionprob,fact =5)
ign_agg <- projectRaster(ign_agg, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
rtp <- rasterToPolygons(ign_agg)
rtp@data$id <- 1:nrow(rtp@data) 
rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')

mymap <- get_map(location = "Canberra" )

bm <- ggmap(mymap)

bm + 
  geom_polygon(data = rtpFortMer, 
               aes(x = long, y = lat, group = group,
                   fill = hdr), 
               size = 0, 
               alpha = 0.5)  + 
  scale_fill_gradientn("RasterValues", colors = topo.colors(255))


###plot of ignition dist in treatment block
intersected_ig_br <- st_intersection(rtp,st_transform(burnblocks,crs= 4326))
palette_ig <- rev(RColorBrewer::brewer.pal(name= "RdYlGn",n = 5))
pal_ign <- colorNumeric(palette = palette_ig,domain = intersected_ig_br$hdr)
rtp <- st_as_sf(rtp)
leaflet(options = leafletOptions(minZoom =  8  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 8) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addPolygons(data = intersected_ig_br,fillColor =~pal_ign(hdr),stroke = FALSE,fillOpacity = 0.5)


## plot of ignition dist in mesh block
intersected_ig_mesh <- st_intersection(rtp,st_transform(meshnlocks,crs=4326))
pal_ign_mesh <- colorNumeric(palette = palette_ig,domain = intersected_ig_mesh$hdr)
leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 9) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
  addPolygons(data = intersected_ig_mesh,fillColor =~pal_ign_mesh(hdr),stroke = FALSE,fillOpacity = 1)


## extract value of ignition raster from center of meshblocks
ign_vals <- raster::extract(ignitionprob,locations[,c(2,3)],method = "bilinear")
ign_vals_df <- cbind(locations,ign_vals)
ign_vals_df <- st_as_sf(ign_vals_df,coords = c("x","y"),crs = 28355)
ign_vals_df <- st_transform(ign_vals_df,crs = 4326)

leaflet(options = leafletOptions(minZoom =  9  , maxZoom =  14)) %>% 
  addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% 
  setView(149,-35.5,zoom = 9) %>% 
  setMaxBounds(lng1=148,
               lat1 =-34,
               lng2 = 151,
               lat2 = -38) %>% 
                addHeatmap(data = ign_vals_df,intensity = ~ign_vals,
                           blur = 10, max = 0.2, radius = 10 )
