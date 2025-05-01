library(tidyverse)
library(sf)
library(geojsonsf)

load("maps_sf.RData")
load("grids_g0_sf.RData")

## Load masks 

masks_5km = st_read("grids5kmHabitatMasks.geojson") %>%
  dplyr::select(GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne)

write.csv(masks_5km %>% st_drop_geometry(),"habitat_mask_data.csv",row.names=F)

## Deccan
# Isolate districts identified for the Deccan gap filling and identify grid cells
# to sample within

deccan_region_1 = c("Adilabad","Yavatmal","Gadchiroli")
deccan_region_2 = c("Nanded","Hingoli","Parbhani","Jalna","Bid",
                    "Latur","Osmanabad","Washim")
deccan_region_3 = c("Malkangiri","Sukma","Bijapur")

dr1_sf = dists_sf %>%
  filter(DISTRICT.NAME %in% deccan_region_1) %>%
  mutate(REGION = 1)
dr2_sf = dists_sf %>%
  filter(DISTRICT.NAME %in% deccan_region_2) %>%
  mutate(REGION = 2)
dr3_sf = dists_sf %>%
  filter(DISTRICT.NAME %in% deccan_region_3) %>%
  mutate(REGION = 3)

# combine districts in a region

dr1_sf_combined = dr1_sf %>%
  group_by(REGION) %>%        # Group by the region column
  summarize(REGION.GEOM = st_union(DISTRICT.GEOM)) %>% ungroup() %>%
  mutate(AREA = as.numeric(st_area(REGION.GEOM)/10^6))

dr2_sf_combined = dr2_sf %>%
  group_by(REGION) %>%        # Group by the region column
  summarize(REGION.GEOM = st_union(DISTRICT.GEOM)) %>% ungroup() %>%
  mutate(AREA = as.numeric(st_area(REGION.GEOM)/10^6))

dr3_sf_combined = dr3_sf %>%
  group_by(REGION) %>%        # Group by the region column
  summarize(REGION.GEOM = st_union(DISTRICT.GEOM)) %>% ungroup() %>%
  mutate(AREA = as.numeric(st_area(REGION.GEOM)/10^6))

# clip grids to each region

dr1_grids_clipped = st_intersection(masks_5km, dr1_sf) %>%
  dplyr::select(REGION,STATE.NAME,DISTRICT.NAME,AREA,
                GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne) %>%
  rename(DISTRICT.AREA = AREA) %>%
  mutate(GRID.AREA = as.numeric(st_area(geometry)/10^6),
         REGION.AREA = dr1_sf_combined$AREA[1])

dr2_grids_clipped = st_intersection(masks_5km, dr2_sf) %>%
  dplyr::select(REGION,STATE.NAME,DISTRICT.NAME,AREA,
                GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne) %>%
  rename(DISTRICT.AREA = AREA) %>%
  mutate(GRID.AREA = as.numeric(st_area(geometry)/10^6),
         REGION.AREA = dr2_sf_combined$AREA[1])

dr3_grids_clipped = st_intersection(masks_5km, dr3_sf) %>%
  dplyr::select(REGION,STATE.NAME,DISTRICT.NAME,AREA,
                GRID_G0,woodland,maskWdl,cropland,maskCrp,one,maskOne) %>%
  rename(DISTRICT.AREA = AREA) %>%
  mutate(GRID.AREA = as.numeric(st_area(geometry)/10^6),
         REGION.AREA = dr3_sf_combined$AREA[1])



## region 1

## select grids to sample

for (i in 1:1000)
{
  set.seed(i)
  
  
  # woodland - 24 per region - 9 in Gadchiroli
  
  # woodland - 16 in woodland masks - 6 in Gadchiroli
  
  w1.a = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, !DISTRICT.NAME %in% c("Gadchiroli"),
           maskWdl == 1) %>%
    sample_n(10)
  w1.b = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, DISTRICT.NAME %in% c("Gadchiroli"),
           maskWdl == 1) %>%
    sample_n(6)
  w1 = w1.a %>% bind_rows(w1.b)
  
  # ONEs - 24 per region - 3 in Gadchiroli (all in ONE masks)
  
  o1.a = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, !DISTRICT.NAME %in% c("Gadchiroli"),
           maskOne == 1) %>%
    sample_n(21)
  o1.b = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, DISTRICT.NAME %in% c("Gadchiroli"),
           maskOne == 1) %>%
    sample_n(3)
  o1 = o1.a %>% bind_rows(o1.b)
  
  # cropland - 40 per region
  
  # cropland - 16 in cropland masks - 4 in Gadchiroli
  
  c1.a = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, !DISTRICT.NAME %in% c("Gadchiroli"),
           maskCrp == 1) %>%
    sample_n(12)
  c1.b = dr1_grids_clipped %>%
    filter(GRID.AREA > 20, DISTRICT.NAME %in% c("Gadchiroli"),
           maskCrp == 1) %>%
    sample_n(4)
  c1 = c1.a %>% bind_rows(c1.b)
  
  # cropland - 16 in selected ONEs
  
  c2 = o1 %>%
    filter(cropland > 0.2) %>%
    sample_n(16)
  
  # cropland - 8 in selected woodland
  
  c3 = w1 %>%
    filter(cropland > 0.2)
  
  # woodland - 8 in cropland masks
  
  w2 = c1 %>%
    filter(woodland > 0.15)
  
  
  if (nrow(c3) >= 8 & nrow(w2) >= 8)
    break
  
  print(i)
}

c3 = c3 %>%
  sample_n(8)

w2 = w2 %>%
  sample_n(8)

w_region_1 = w1 %>%
  bind_rows(w2) 
c_region_1 = c1 %>%
  bind_rows(c2) %>%
  bind_rows(c3)
o_region_1 = o1

w_region_1_all = dr1_grids_clipped %>%
  filter(maskWdl == 1)
c_region_1_all = dr1_grids_clipped %>%
  filter(maskCrp == 1)
o_region_1_all = dr1_grids_clipped %>%
  filter(maskOne == 1)

# replace with final grids

deccan_region_1_Wdl_final = read.csv("Deccan/deccan_region_1_Wdl_final.csv")
deccan_region_1_Crp_final = read.csv("Deccan/deccan_region_1_Crp_final.csv")
deccan_region_1_One_final = read.csv("Deccan/deccan_region_1_One_final.csv")

w_region_1 = dr1_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_1_Wdl_final$GRID_G0)
c_region_1 = dr1_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_1_Crp_final$GRID_G0)
o_region_1 = dr1_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_1_One_final$GRID_G0)

# add replacement grids

# changes_region_1 = read.csv("deccan_replaced_grids.csv") %>%
#   mutate(GRID_G0 = as.character(GRID_G0),
#          GRID_G0_NEW = as.character(GRID_G0_NEW))
# 
# geom_changes_region_1 = dr1_grids_clipped %>%
#   filter(GRID_G0 %in% changes_region_1$GRID_G0_NEW) %>%
#   rename(GRID_G0_NEW = GRID_G0)
# 
# w_region_1_add = w_region_1 %>%
#   filter(GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   dplyr::select(GRID_G0) %>%
#   left_join(changes_region_1) %>%
#   dplyr::select(-GRID_G0) %>%
#   st_drop_geometry() %>%
#   left_join(geom_changes_region_1) %>%
#   rename(GRID_G0 = GRID_G0_NEW) %>%
#   dplyr::select(names(w_region_1))
# 
# w_region_1 = w_region_1 %>%
#   filter(!GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   bind_rows(w_region_1_add)
#   
# c_region_1_add = c_region_1 %>%
#   filter(GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   dplyr::select(GRID_G0) %>%
#   left_join(changes_region_1) %>%
#   dplyr::select(-GRID_G0) %>%
#   st_drop_geometry() %>%
#   left_join(geom_changes_region_1) %>%
#   rename(GRID_G0 = GRID_G0_NEW) %>%
#   dplyr::select(names(c_region_1))
# 
# c_region_1 = c_region_1 %>%
#   filter(!GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   bind_rows(c_region_1_add)
# 
# o_region_1_add = o_region_1 %>%
#   filter(GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   dplyr::select(GRID_G0) %>%
#   left_join(changes_region_1) %>%
#   dplyr::select(-GRID_G0) %>%
#   st_drop_geometry() %>%
#   left_join(geom_changes_region_1) %>%
#   rename(GRID_G0 = GRID_G0_NEW) %>%
#   dplyr::select(names(o_region_1))
# 
# o_region_1 = o_region_1 %>%
#   filter(!GRID_G0 %in% changes_region_1$GRID_G0) %>%
#   bind_rows(o_region_1_add)


row.names(w_region_1) = make.unique(as.character(w_region_1$GRID_G0))
row.names(w_region_1_all) = make.unique(as.character(w_region_1_all$GRID_G0))
row.names(c_region_1) = make.unique(as.character(c_region_1$GRID_G0))
row.names(c_region_1_all) = make.unique(as.character(c_region_1_all$GRID_G0))
row.names(o_region_1) = make.unique(as.character(o_region_1$GRID_G0))
row.names(o_region_1_all) = make.unique(as.character(o_region_1_all$GRID_G0))

## region 2

## select grids to sample

for (i in 1:1000)
{
  set.seed(i)
  
  
  # woodland - 24 per region
  
  # woodland - 16 in woodland masks
  
  w1 = dr2_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskWdl == 1) %>%
    sample_n(16)

  
  # ONEs - 24 per region
  
  o1 = dr2_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskOne == 1) %>%
    sample_n(24)
  
  # cropland - 40 per region
  
  # cropland - 16 in cropland masks
  
  c1 = dr2_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskCrp == 1) %>%
    sample_n(16)

  # cropland - 16 in selected ONEs
  
  c2 = o1 %>%
    filter(cropland > 0.2) %>%
    sample_n(16)
  
  # cropland - 8 in selected woodland
  
  c3 = w1 %>%
    filter(cropland > 0.2)
  
  # woodland - 8 in cropland masks
  
  w2.a = c1 %>%
    filter(woodland > 0.15)
  
  
  if (nrow(c3) >= 8 & nrow(w2.a) >= 3)
  {
    w2.b = dr2_grids_clipped %>%
      filter(GRID.AREA > 20,
             maskCrp == 1) %>%
      filter(woodland > 0.15) %>%
      sample_n(8-nrow(w2.a))
    w2 =  w2.a %>% bind_rows(w2.b)
    break
  }
  
  print(i)
}

c3 = c3 %>%
  sample_n(8)

w2 = w2 %>%
  sample_n(8)

w_region_2 = w1 %>%
  bind_rows(w2) 
c_region_2 = c1 %>%
  bind_rows(c2) %>%
  bind_rows(c3)
o_region_2 = o1

w_region_2_all = dr2_grids_clipped %>%
  filter(maskWdl == 1)
c_region_2_all = dr2_grids_clipped %>%
  filter(maskCrp == 1)
o_region_2_all = dr2_grids_clipped %>%
  filter(maskOne == 1)

# replace with final grids

deccan_region_2_Wdl_final = read.csv("Deccan/deccan_region_2_Wdl_final.csv")
deccan_region_2_Crp_final = read.csv("Deccan/deccan_region_2_Crp_final.csv")
deccan_region_2_One_final = read.csv("Deccan/deccan_region_2_One_final.csv")

w_region_2 = dr2_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_2_Wdl_final$GRID_G0)
c_region_2 = dr2_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_2_Crp_final$GRID_G0)
o_region_2 = dr2_grids_clipped %>%
  filter(GRID_G0 %in% deccan_region_2_One_final$GRID_G0)

row.names(w_region_2) = make.unique(as.character(w_region_2$GRID_G0))
row.names(w_region_2_all) = make.unique(as.character(w_region_2_all$GRID_G0))
row.names(c_region_2) = make.unique(as.character(c_region_2$GRID_G0))
row.names(c_region_2_all) = make.unique(as.character(c_region_2_all$GRID_G0))
row.names(o_region_2) = make.unique(as.character(o_region_2$GRID_G0))
row.names(o_region_2_all) = make.unique(as.character(o_region_2_all$GRID_G0))


## region 3

## select grids to sample

for (i in 1:1000)
{
  set.seed(i)
  
  
  # woodland - 24 per region
  
  # woodland - 16 in woodland masks
  
  w1 = dr3_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskWdl == 1) %>%
    sample_n(16)
  
  
  # ONEs - 24 per region
  
  o1 = dr3_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskOne == 1) %>%
    sample_n(24)
  
  # cropland - 40 per region
  
  # cropland - 16 in cropland masks
  
  c1 = dr3_grids_clipped %>%
    filter(GRID.AREA > 20,
           maskCrp == 1) %>%
    sample_n(16)
  
  # cropland - 16 in selected ONEs
  
  c2.a = o1 %>%
    filter(cropland > 0.2)
  
  # cropland - 8 in selected woodland
  
  c3.a = w1 %>%
    filter(cropland > 0.2)
  
  # woodland - 8 in cropland masks
  
  w2 = c1 %>%
    filter(woodland > 0.15)
  
  
  if (nrow(c2.a) >= 14 & nrow(c3.a) >= 4 & nrow(w2) >= 8)
  {
    c2.b = dr3_grids_clipped %>%
      filter(GRID.AREA > 20,
             maskOne == 1) %>%
      filter(cropland > 0.2) %>%
      sample_n(16-nrow(c2.a))
    c2 =  c2.a %>% bind_rows(c2.b)
    
    c3.b = dr3_grids_clipped %>%
      filter(GRID.AREA > 20,
             maskWdl == 1) %>%
      filter(cropland > 0.2) %>%
      sample_n(8-nrow(c3.a))
    c3 =  c3.a %>% bind_rows(c3.b)
    break
  }
  
  print(i)
}

c2 = c2 %>%
  sample_n(16)

c3 = c3 %>%
  sample_n(8)

w2 = w2 %>%
  sample_n(8)

w_region_3 = w1 %>%
  bind_rows(w2) 
c_region_3 = c1 %>%
  bind_rows(c2) %>%
  bind_rows(c3)
o_region_3 = o1

w_region_3_all = dr3_grids_clipped %>%
  filter(maskWdl == 1)
c_region_3_all = dr3_grids_clipped %>%
  filter(maskCrp == 1)
o_region_3_all = dr3_grids_clipped %>%
  filter(maskOne == 1)

row.names(w_region_3) = make.unique(as.character(w_region_3$GRID_G0))
row.names(w_region_3_all) = make.unique(as.character(w_region_3_all$GRID_G0))
row.names(c_region_3) = make.unique(as.character(c_region_3$GRID_G0))
row.names(c_region_3_all) = make.unique(as.character(c_region_3_all$GRID_G0))
row.names(o_region_3) = make.unique(as.character(o_region_3$GRID_G0))
row.names(o_region_3_all) = make.unique(as.character(o_region_3_all$GRID_G0))



## plotting and creating geoJSONs

require(mapview)
mapviewOptions(fgb = FALSE)

wr1_all_map = mapView(w_region_1_all, zcol = NULL, 
                         map.types = c("Esri.WorldImagery"),
                         layer.name = "Region 1 Woodland Mask",
                         popup = leafpop::popupTable(w_region_1_all,
                                                     c("REGION","STATE.NAME","DISTRICT.NAME",
                                                       "DISTRICT.AREA","GRID_G0","woodland",
                                                       "maskWdl","cropland","maskCrp","one",
                                                       "maskOne","GRID.AREA","REGION.AREA"),
                                                     feature.id=FALSE,
                                                     row.numbers=FALSE),
                         alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

wr1_selected_map = mapView(w_region_1, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 1 Selected Woodland",
                      popup = leafpop::popupTable(w_region_1,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

cr1_all_map = mapView(c_region_1_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 1 Cropland Mask",
                      popup = leafpop::popupTable(c_region_1_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

cr1_selected_map = mapView(c_region_1, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 1 Selected Cropland",
                           popup = leafpop::popupTable(c_region_1,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

or1_all_map = mapView(o_region_1_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 1 ONE Mask",
                      popup = leafpop::popupTable(o_region_1_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

or1_selected_map = mapView(o_region_1, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 1 Selected ONE",
                           popup = leafpop::popupTable(o_region_1,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

wr2_all_map = mapView(w_region_2_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 2 Woodland Mask",
                      popup = leafpop::popupTable(w_region_2_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

wr2_selected_map = mapView(w_region_2, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 2 Selected Woodland",
                           popup = leafpop::popupTable(w_region_2,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

cr2_all_map = mapView(c_region_2_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 2 Cropland Mask",
                      popup = leafpop::popupTable(c_region_2_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

cr2_selected_map = mapView(c_region_2, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 2 Selected Cropland",
                           popup = leafpop::popupTable(c_region_2,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

or2_all_map = mapView(o_region_2_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 2 ONE Mask",
                      popup = leafpop::popupTable(o_region_2_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

or2_selected_map = mapView(o_region_2, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 2 Selected ONE",
                           popup = leafpop::popupTable(o_region_2,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

wr3_all_map = mapView(w_region_3_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 3 Woodland Mask",
                      popup = leafpop::popupTable(w_region_3_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

wr3_selected_map = mapView(w_region_3, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 3 Selected Woodland",
                           popup = leafpop::popupTable(w_region_3,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#00FF00")

cr3_all_map = mapView(c_region_3_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 3 Cropland Mask",
                      popup = leafpop::popupTable(c_region_3_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

cr3_selected_map = mapView(c_region_3, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 3 Selected Cropland",
                           popup = leafpop::popupTable(c_region_3,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#FFFF00")

or3_all_map = mapView(o_region_3_all, zcol = NULL, 
                      map.types = c("Esri.WorldImagery"),
                      layer.name = "Region 3 ONE Mask",
                      popup = leafpop::popupTable(o_region_3_all,
                                                  c("REGION","STATE.NAME","DISTRICT.NAME",
                                                    "DISTRICT.AREA","GRID_G0","woodland",
                                                    "maskWdl","cropland","maskCrp","one",
                                                    "maskOne","GRID.AREA","REGION.AREA"),
                                                  feature.id=FALSE,
                                                  row.numbers=FALSE),
                      alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

or3_selected_map = mapView(o_region_3, zcol = NULL, 
                           map.types = c("Esri.WorldImagery"),
                           layer.name = "Region 3 Selected ONE",
                           popup = leafpop::popupTable(o_region_3,
                                                       c("REGION","STATE.NAME","DISTRICT.NAME",
                                                         "DISTRICT.AREA","GRID_G0","woodland",
                                                         "maskWdl","cropland","maskCrp","one",
                                                         "maskOne","GRID.AREA","REGION.AREA"),
                                                       feature.id=FALSE,
                                                       row.numbers=FALSE),
                           alpha.regions = 0, lwd = 5, legend = NULL, color = "#F7991E")

st_write(w_region_1_all, "Deccan/Region 1 Woodland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(w_region_1, "Deccan/Region 1 Selected Woodland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_1_all, "Deccan/Region 1 Cropland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_1, "Deccan/Region 1 Selected Cropland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_1_all, "Deccan/Region 1 ONE Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_1, "Deccan/Region 1 Selected ONE.geojson", driver = "GeoJSON", delete_dsn = TRUE)

st_write(w_region_2_all, "Deccan/Region 2 Woodland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(w_region_2, "Deccan/Region 2 Selected Woodland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_2_all, "Deccan/Region 2 Cropland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_2, "Deccan/Region 2 Selected Cropland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_2_all, "Deccan/Region 2 ONE Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_2, "Deccan/Region 2 Selected ONE.geojson", driver = "GeoJSON", delete_dsn = TRUE)

st_write(w_region_3_all, "Deccan/Region 3 Woodland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(w_region_3, "Deccan/Region 3 Selected Woodland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_3_all, "Deccan/Region 3 Cropland Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(c_region_3, "Deccan/Region 3 Selected Cropland.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_3_all, "Deccan/Region 3 ONE Mask.geojson", driver = "GeoJSON", delete_dsn = TRUE)
st_write(o_region_3, "Deccan/Region 3 Selected ONE.geojson", driver = "GeoJSON", delete_dsn = TRUE)


deccan_grids = wr1_all_map + wr1_selected_map + 
  cr1_all_map + cr1_selected_map +
  or1_all_map + or1_selected_map +
  wr2_all_map + wr2_selected_map + 
  cr2_all_map + cr2_selected_map +
  or2_all_map + or2_selected_map +
  wr3_all_map + 
  cr3_all_map +
  or3_all_map

mapshot(deccan_grids, "Deccan/deccan_grids.html")
