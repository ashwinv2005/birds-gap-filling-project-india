library(tidyverse)
library(sf)
library(geojsonsf)

load("maps_sf.RData")
load("grids_g0_sf.RData")

## Load masks 

masks_5km = st_read("grids5kmHabitatMasks.geojson") %>%
  dplyr::select(GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne)

dr = states_sf %>%
  filter(STATE.NAME %in% "Telangana")

dr_dists = dists_sf %>%
  filter(STATE.NAME %in% "Telangana")

dr_grids_clipped = st_intersection(masks_5km, dr) %>%
  dplyr::select(STATE.NAME,AREA,
                GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne) %>%
  mutate(GRID.AREA = as.numeric(st_area(geometry)/10^6))

## plotting and creating geoJSONs

require(mapview)
mapviewOptions(fgb = FALSE)

dr_grids_map = mapView(dr_grids_clipped, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Telangana grids",
                      popup = leafpop::popupTable(dr_grids_clipped,
                                                  c("STATE.NAME",
                                                    "AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 2, legend = NULL, color = "#660000")

ts_dists_map = mapView(dr_dists, zcol = NULL,
                   map.types = c("Esri.WorldImagery"),
                   layer.name = "Telangana districts", 
                   popup = leafpop::popupTable(dr_dists,
                                               c("DISTRICT.NAME"), 
                                               feature.id=FALSE, 
                                               row.numbers=FALSE),
                   alpha.regions = 0, lwd = 2, legend = NULL, color = "white")

dr_grids_map = dr_grids_map + ts_dists_map
  

st_write(dr_grids_clipped, "Telangana 5km grids.geojson", driver = "GeoJSON", delete_dsn = TRUE)
mapshot(dr_grids_map, "Telangana_grids.html")

