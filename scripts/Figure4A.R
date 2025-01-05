
#----Mapping ocean uses - Perspective piece----#
#
#----Follows on from 'Ocean_uses_AUS_2.R'
#
# 14/11/2023


#--------At start up--------#

library(tidyverse)
library(here)
library(sf)
library(tmap)
library(tmaptools)
library(gridExtra)
library(terra)
library(spatialEco)
library(scales)
library(stars)
library(concaveman)
library(rnaturalearth)
library(rnaturalearthdata)

sf_use_s2(F)

#---------------------------#

# Import world sf dataframe
world <- ne_countries(scale = 'medium', returnclass = 'sf')

# Filter for Australia
AUS <- subset(world, admin == 'Australia')

### Link to figshare for 'uses_sf_eez'
### https://figshare.com/articles/dataset/Kuempel_offshore_wind_CIA/26506846
# Import ocean use layers
uses_sf_eez <- readRDS(here('output_data/ocean_uses/uses_sf_eez')) #download this layer from the figshare link in the repo

# Create list of distinct uses
names(uses_sf_eez)
no_uses_sf_eez <- uses_sf_eez[c('cables_active_csiro', 'oil_lease_areas_AUS_NOPTA',
                                'rec_boats_AUS_csiro', 
                                'updatedpetroleumpipelines_AUS_aodn',
                                'Ports_terms', 'wind_farms_dec', 
                                'fishing_all', 'WindFarm_WA')]

#-----Additional layers to add-----#

### SHIPPING

## Download data from the following link:
## https://www.operations.amsa.gov.au/Spatial/DataServices/DigitalData

# Import shipping layer
# shipping <- st_read(here('raw_data/ocean_uses/XXXXXXXX'))

# Add shipping to list
no_uses_sf_eez <- append(no_uses_sf_eez, lst(shipping))


### RECREATION PARKS

## Download data from the following link:
## https://www.dcceew.gov.au/environment/land/nrs/science/capad

## From 'capad' dataset - IUCN category 5:
## "Protected Landscape / Seascape: protected area managed 
## mainly for landscape/seascape conservation and recreation"

# # Import
# rec_parks <- st_read(here('raw_data/ocean_uses/XXXXXX'))

# Column names
colnames(rec_parks)

# IUCN column
unique(rec_parks$IUCN)

# Filter data to IUCN V
rec_parks <- filter(rec_parks, IUCN == 'V')

# Add to list
no_uses_sf_eez <- append(no_uses_sf_eez, lst(rec_parks))


### MPAs and IPAs - 2022

## Download link:
## https://www.dcceew.gov.au/environment/land/nrs/science/capad#:~:text=CAPAD%20is%20a%20textual%20and,cent)%20of%20the%20Australian%20landmass.

# # Import MPA data
# MPAs <- st_read(here('raw_data/ocean_uses/#####'))

# Add to list
no_uses_sf_eez <- append(no_uses_sf_eez, lst(MPAs))

# Import IPA data
IPAs <- st_read(here('raw_data/ocean_uses/ipa_dedicated.shp'))

# Add to list
no_uses_sf_eez <- append(no_uses_sf_eez, lst(IPAs))

# # Save
# saveRDS(no_uses_sf_eez, here('output_data/ocean_uses/no_uses_sf_eez'))


#-----Rasterise to EEZ extent-----#

# EEZ extent 
# xmin: 109.2335 ymin: -47.1936 xmax: 163.1921 ymax: -8.8819

# Function to rasterise
r_fun <- function(x) {
  rasterize(x, rast(xmin = 109.2335, xmax = 163.1921,
                    ymin = -47.1936, ymax = -8.8819),
            field = 1, background = 0, touches = T)
}

# Convert 'fishing_all' to sf dataframe
no_uses_sf_eez$fishing_all <- st_as_sf(no_uses_sf_eez$fishing_all)

# Rasterise all
no_uses_r_lst <- lapply(no_uses_sf_eez, r_fun)


## AQUACULTURE

# Import aquaculture data
AC_pts <- st_read(here('raw_data/ocean_uses/All_Australian_aquaculture_point_data.shp'))
AC_poly <- st_read(here('raw_data/ocean_uses/All_Australian_aquaculture_polygon_data.shp'))

# Remove NT points
AC_pts <- filter(AC_pts, State != 'NT')

# Erase points outside of Australia polygon
AC_pts_erase <- erase(vect(AC_pts), vect(AUS))
AC_poly_erase <- erase(vect(AC_poly), vect(AUS))

# Convert to sf
AC_pts_erase_sf <- st_as_sf(AC_pts_erase)
AC_poly_erase_sf <- st_as_sf(AC_poly_erase)

# project
AC_pts_erase_sf<- st_transform(AC_pts_erase_sf, '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84')
AC_poly_erase_sf <- st_transform(AC_poly_erase_sf, '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84')

# # Save polygon file
# st_write(AC_poly_erase_sf, here('output_data/ocean_uses/AC_poly_erase_sf.gpkg'))



## Create 1 Km buffer for points

# Buffer function
buffer_1km <- function(x) {
  st_buffer(x, dist = units::set_units(1, km))
}

# Add buffer
AC_buff_erase_sf <- buffer_1km(AC_pts_erase_sf)

# Combine all AC polygons
AC_poly_all <- bind_rows(AC_poly_erase_sf, AC_buff_erase_sf)

# Add column of 1's
AC_poly_all$field <- 1

# Transform crs
AC_poly_all <- st_transform(AC_poly_all, 'EPSG:4326')

# # Save
# st_write(AC_poly_all, here('output_data/ocean_uses/AC_poly_all.gpkg'))

# Rasterise
AC_r <- r_fun(AC_poly_all)

# Resample aquaculture to align extent
AC_r_rsmp <- resample(AC_r, no_uses_r_lst$cables_active_csiro,
                      method = 'near')

# Add aquaculture to raster list
no_uses_r_lst <- append(no_uses_r_lst, lst(AC_r_rsmp))

# # Check
# lapply(no_uses_r_lst, summary)

# Overlay using the sum of overlapping grid cells (all 1)
no_uses_r <- rast(no_uses_r_lst)
no_uses_r <- app(no_uses_r, sum)

# Mask uses raster with AUS to remove land areas
uses_mask <- mask(no_uses_r, vect(AUS), inverse = T)


## Crop to EEZ

# Import EEZ
eez <- st_read(here('raw_data/ocean_uses/EEZLimitContinental.gpkg'))

# Filter to cover relevant area
eez <- filter(
 eez, 
 OBJECTID == 1 |
   OBJECTID == 8 |
   OBJECTID == 10 |
   OBJECTID == 11 |
   OBJECTID == 12)
plot(st_geometry(eez))

# Create boundary
eez_boundary <- concaveman(eez)
 
# Create boundary
eez_boundary <- concaveman(eez)

# Remove Z dimension
eez_boundary <- st_zm(eez_boundary)

# Crop
no_uses_r_final <- mask(uses_mask, vect(eez_boundary))

# # Save
# writeRaster(no_uses_r_final, here('output_data/ocean_uses/no_uses_r_final.tif'))


#-----FIGURES-----#

# Map of no_uses
no_uses_map <- 
  tm_shape(no_uses_r_final) +
  tm_raster(title = 'no_uses',
            palette = 'YlGn',
            style = 'cont',
            legend.reverse = T) +
  tm_shape(AUS) +
  tm_polygons(col = 'grey70') +
  tm_layout(legend.outside = T,
            legend.outside.size = 0.1,
            legend.title.size = 2,
            legend.text.size = 0.8) +
  tm_compass(type = 'arrow', 
             size = 1.5, 
             text.size = 1, 
             position=c(0.9, 0.88)) +
  tm_scale_bar(breaks = c(0,400,800,1200),
               text.size = 0.7, 
               position=c(0.02,0.0001))
no_uses_map

# Save
tmap_save(no_uses_map, here('figures/Figure4A_num_ocean_uses_map.png'))


### Total area occupied by varying number of users across Australia

# Calculate area covered by each value
no_uses_area <- expanse(no_uses_r_final, unit = 'km', byValue = T)

# Filter to exclude zeros
no_uses_area_2 <- filter(no_uses_area, value != 0)

# Percent of EEZ containing one use
(no_uses_area_2$area[1]/(sum(no_uses_area_2$area)))*100

# Percent of EEZ containing multiple uses
(sum(no_uses_area_2$area[2:6])/(sum(no_uses_area_2$area)))*100

# Area (Km2) of EEZ containing multiple uses
sum(no_uses_area_2$area[2:6])


### Calculate area for each industry

# Add aquaculture to sf list
no_uses_sf_eez <- append(no_uses_sf_eez, lst(AC_poly_all))

# Project coordinates
no_uses_sf_prj <- lapply(no_uses_sf_eez, 
                         st_transform, 
                         '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84')

# Create list of points and lines to buffer
pts_lines <- no_uses_sf_prj[c('cables_active_csiro',
                              'updatedpetroleumpipelines_AUS_aodn',
                              'Ports_terms', 'shipping')]

# # Buffer function
# buffer_1km <- function(x) {
#   st_buffer(x, dist = units::set_units(1, km))
# }

# Create 1 Km buffer
pts_lines <- lapply(pts_lines, buffer_1km)

# Remove original points and lines from list
no_uses_sf_prj_polys <- no_uses_sf_prj[-c(1,4,5,9)]

# Add buffered points and lines to list
no_uses_sf_prj_all <- c(no_uses_sf_prj_polys, pts_lines)

# Make the names 'prettier'
names(no_uses_sf_prj_all) <- c('Oil lease areas', 'Recreational boat use', 
                               'Wind farm areas', 'Commercial fishing',
                               'WindFarm_WA', 'Recreational parks',
                               'MPAs', 'IPAs', 'Aquaculture',
                               'Underwater cables', 'Petroleum pipelines',
                               'Ports and terminals', 'Shipping')

# Add layer name to each dataframe
no_uses_sf_prj_all <- no_uses_sf_prj_all %>%  
  map2(names(no_uses_sf_prj_all), ~mutate(.x, layer = .y))


### Dissolve polygons

## Geometry names do not match
## Will do them separately

# Make geoms valid
no_uses_sf_prj_all <- lapply(no_uses_sf_prj_all, st_make_valid)

# # Save list
# saveRDS(no_uses_sf_prj_all, here('output_data', 'ocean_uses', 'no_uses_sf_prj_all'))


## Rename geometry columns

# Create function
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}

# Rename
no_uses_geoms <- lapply(no_uses_sf_prj_all, rename_geometry, 'geometry')

# Get layer column with geom
uses_area_geoms <- bind_rows(lapply(
  no_uses_geoms, "[", 'layer'))

# Dissolve
uses_area_diss <- uses_area_geoms %>%
  group_by(layer) %>% 
  summarize(geometry = st_union(geometry))

# Add column with area
uses_area_diss$area_km2 <- 
  st_area(uses_area_diss) %>% 
  units::set_units('km2')

# Crop to EEZ
uses_area_diss <- st_crop(uses_area_diss, eez_boundary)
test <- st_crop(uses_area_diss, st_transform(eez_boundary, 
                '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84'))

# # Save
# st_write(uses_area_diss, here('output_data/ocean_uses/uses_area_diss.gpkg'))


## Create new dataframe to combine the wind farm data

# # Check dataframe positions
# tail(uses_area_diss)

# Dataframe columns and values
layer <- 'Wind farm areas'
area_km2 <- uses_area_diss[[12,2]] + uses_area_diss[[13,2]]

# Create dataframe
wind_farms_area <- data.frame(layer, area_km2)

# Remove individual wind entries and join wind_farms_area with all data
uses_area_final <- rbind(st_drop_geometry(uses_area_diss)[1:11, ],
                         wind_farms_area)

# Convert area_km2 to numeric
uses_area_final$area_km2 <- as.numeric(uses_area_final$area_km2)

# # Save
# st_write(uses_area_final, here('output_data/ocean_uses/uses_area_final.gpkg'))
