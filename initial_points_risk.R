library(sf)
library(raster)
library(rgdal)
library(here)
library(tidyverse)
library(WildfireRisk)
library(RSQLite)
library(ggplot2)


tsf <- raster( here("test_leaflet/data/act_rasters/actplus_tsf17/hdr.adf") )

forest <- raster( here("test_leaflet/data/act_rasters/actfor_comb/hdr.adf") )

burnblocks <- st_read(here("test_leaflet/data/ACT_Test_Data/ACTBurnBlocks_gt1.shp"))

ignitionprob <- raster(here("test_leaflet/data/act_rasters/ignition/act_igden_n2/hdr.adf"))

locations <- read.csv( here("test_leaflet/data/ACT_Test_Data/act_cens11_centres.CSV") )

locations[, 1] <- stringr::str_trim( sprintf("%12.0f", locations[, 1]) )



length_fun <- function(n) 100 + rexp_truncated(n, 1/5000, 20000 - 100)
lines <- make_scan_lines(locations, 80, lengths = length_fun,crs = tsf)

risk.lines <- calculate_risk(lines, tsf, forest, sample.spacing = 100)

risk.loc <- summarize_location_risk(risk.lines)

st_write(risk.loc,"initial_risk.shp")
