
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

sf_use_s2(F)

# Import Australia basemap, project, and add labels
AUS <- st_read(here('data', 'STE_2016_AUST.gpkg'))
AUS <- st_transform(AUS, 'EPSG:4326')
labs <- c('NSW','VIC','QLD','SA','WA','TAS','NT','','')
AUS$labs <- labs

# Bounding box
new_bbox <- bb(c(109.2335, -47.19364, 163.1921, -8.881893))

# Import R objects
no.uses_r_final <- rast(here('output_data', 'ocean_uses', 'no.uses_r_final.tif'))
no.uses_sf_prj_all <- readRDS(here('output_data', 'ocean_uses', 'no_uses_sf_prj_all'))
uses_area_final <- st_read(here('output_data', 'ocean_uses', 'uses_area_final.gpkg'))

#---------------------------#

# Import ocean use layers
uses_sf_eez <- readRDS(here('output_data', 'ocean_uses', 'uses_sf_eez'))

# Create list of distinct uses
names(uses_sf_eez)
no.uses_sf_eez <- uses_sf_eez[c('cables_active_csiro', 'oil_lease_areas_AUS_NOPTA',
                                'rec_boats_AUS_csiro', 
                                'updatedpetroleumpipelines_AUS_aodn',
                                'Ports_terms', 'wind_farms_dec', 
                                'fishing_all', 'WindFarm_WA')]

#-----Additional layers to add-----#

## SHIPPING

# Import shipping layer
shipping <- st_read(here('raw_data', 'cts_srr_08_2023_pt.shp'))

# Add shipping to list
no.uses_sf_eez <- append(no.uses_sf_eez, lst(shipping))


## RECREATION PARKS
## From 'capad' dataset - IUCN category 5:
## "Protected Landscape / Seascape: protected area managed 
## mainly for landscape/seascape conservation and recreation"

# Import
rec_parks <- st_read(here('raw_data', 'capad_2016_marine.shp'))

# Column names
colnames(rec_parks)

# IUCN column
unique(rec_parks$IUCN)

# Filter data to IUCN V
rec_parks <- filter(rec_parks, IUCN == 'V')

# Add to list
no.uses_sf_eez <- append(no.uses_sf_eez, lst(rec_parks))


## MPAs and IPAs - 2022

# Import MPA data
MPAs <- st_read(here('raw_data', 'Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp'))

# Add to list
no.uses_sf_eez <- append(no.uses_sf_eez, lst(MPAs))

# Import IPA data
IPAs <- st_read(here('raw_data', 'ipa_dedicated.shp'))

# Add to list
no.uses_sf_eez <- append(no.uses_sf_eez, lst(IPAs))

# Save
saveRDS(no.uses_sf_eez, here('output_data','ocean_uses', 'no_uses_sf_eez'))


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
no.uses_sf_eez$fishing_all <- st_as_sf(no.uses_sf_eez$fishing_all)

# Rasterise all
no.uses_r_lst <- lapply(no.uses_sf_eez, r_fun)


## AQUACULTURE

# Import aquaculture data
AC_pts <- st_read(here('raw_data', 'Aquaculture', 'All_Australian_aquaculture_point_data.shp'))
AC_poly <- st_read(here('raw_data', 'Aquaculture', 'All_Australian_aquaculture_polygon_data.shp'))

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

# Save polygon file
st_write(AC_poly_erase_sf, here('output_data', 'ocean_uses', 'AC_poly_erase_sf.gpkg'))

# Check points
tm_shape(AUS, bbox = bb(
  filter(AUS, STE_NAME16 == 'Northern Territory'))) +
  tm_polygons(alpha = 0.3, border.alpha = 0.3) +
  tm_shape(AC_pts_erase_sf) +
  tm_dots(col = 'red')

# Check polygons - closer look at NSW
tm_shape(AUS, 
         bbox = bb(
           filter(AUS,STE_NAME16 == 'New South Wales'))) +
  tm_polygons(alpha = 0.3,
              border.col = 'red',
              border.alpha = 0.3) +
  tm_shape(AC_poly_erase_sf) +
  tm_polygons()


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

# Save
st_write(AC_poly_all, here('output_data', 'ocean_uses', 'AC_poly_all.gpkg'))

# Rasterise
AC_r <- r_fun(AC_poly_all)

# Resample aquaculture to align extent
AC_r_rsmp <- resample(AC_r, no.uses_r_lst$cables_active_csiro,
                      method = 'near')

# Add aquaculture to raster list
no.uses_r_lst <- append(no.uses_r_lst, lst(AC_r_rsmp))

# # Check
# lapply(no.uses_r_lst, summary)

# Overlay using the sum of overlapping grid cells (all 1)
no.uses_r <- rast(no.uses_r_lst)
no.uses_r <- app(no.uses_r, sum)

# Mask uses raster with AUS to remove land areas
uses_mask <- mask(no.uses_r, vect(AUS), inverse = T)


## Crop to EEZ

# Import EEZ
eez <- st_read(here('raw_data', 'EEZLimitContinental.gpkg'))

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
no.uses_r_final <- mask(uses_mask, vect(eez_boundary))

# Save
writeRaster(no.uses_r_final, here('output_data', 'ocean_uses', 'no.uses_r_final.tif'))


#-----FIGURES-----#

# Map of no.uses
no.uses_map <- 
  tm_shape(no.uses_r_final) +
  tm_raster(title = 'No.uses',
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
no.uses_map

# Save
tmap_save(no.uses_map, here('figures', 'no_uses_map_persp.png'))


### Total area occupied by varying number of users across Australia

# Calculate area covered by each value
no.uses_area <- expanse(no.uses_r_final, unit = 'km', byValue = T)

# Filter to exclude zeros
no.uses_area_2 <- filter(no.uses_area, value != 0)

# Percent of EEZ containing one use
(no.uses_area_2$area[1]/(sum(no.uses_area_2$area)))*100

# Percent of EEZ containing multiple uses
(sum(no.uses_area_2$area[2:6])/(sum(no.uses_area_2$area)))*100

# Area (Km2) of EEZ containing multiple uses
sum(no.uses_area_2$area[2:6])

# Barplot with no.uses on x axis
no.uses_bp <- ggplot(no.uses_area_2) +
  geom_bar(aes(x = as.factor(value), y = area/1000000), 
           stat = 'identity', col = 'black', fill = 'green4') +
  labs(x = '\nNo.uses', y = expression('Total area' ~ '(mil' ~ Km^2*')')) +
  theme_classic() +
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=12)) + 
  scale_y_continuous(labels = comma)
no.uses_bp

# Save
ggsave(here('figures', 'no_uses_bp.png'), no.uses_bp,
       width = 8, height = 5)


# ### Area of use for each industry
# 
# ## Change zeros to NA - lets go back to our sf list
# 
# # Second function to rasterise
# r_fun_2 <- function(x) {
#   rasterize(x, rast(xmin = 109.2335, xmax = 163.1921,
#                     ymin = -47.1936, ymax = -8.8819),
#             field = 1, 
#             background = NA, # NA instead of zero for area calc
#             touches = T)
# }
# 
# # Rasterise
# no.uses_r_lst_2 <- lapply(no.uses_sf_eez, r_fun_2)
# 
# # Make the names 'prettier'
# names(no.uses_r_lst_2) <- c('Underwater cables',
#                             'Oil lease areas',
#                             'Recreational boat use', 'Petroleum pipelines',
#                             'Ports and terminals', 'Wind farm areas', 
#                             'Commercial fishing', 'WindFarm_WA', 'Shipping',
#                             'Recreational parks', 'Aquaculture',
#                             'MPAs', 'IPAs')
# 
# # Calculate area for each industry - rasters
# industry_area_lst <- lapply(no.uses_r_lst_2, expanse, unit = 'km')
# 
# # Add layer name to each dataframe
# industry_area_lst  <- industry_area_lst %>%  
#   map2(names(industry_area_lst), ~mutate(.x, layer = .y))
# 
# # Sum the wind farm areas
# industry_area_lst$`Wind farm areas`$area <- 
#   industry_area_lst$`Wind farm areas`$area + industry_area_lst$WindFarm_WA$area
# 
# # Remove WindFarm_WA from list (8th element)
# industry_area_lst <- industry_area_lst[-8]
# 
# # Bind all data
# industry_area_df <- bind_rows(industry_area_lst)

# # Barplot with industries on x axis and area on y axis
# area_bp <- ggplot(industry_area_df) +
#   geom_bar(aes(x = layer, y = area), 
#            stat = 'identity', col = 'black', fill = 'green4') +
#   labs(x = '', y = expression('Total area' ~ (Km^2))) +
#   theme_classic() +
#   theme(axis.title=element_text(size=15),
#         axis.text=element_text(size=12),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
#   scale_y_continuous(labels = comma)
# area_bp
# 
# # Save
# ggsave(here('output', 'area_bp.png'), area_bp,
#        width = 8, height = 5)


## Calculate area for each industry - sf

# Add aquaculture to sf list
no.uses_sf_eez <- append(no.uses_sf_eez, lst(AC_poly_all))

# Project coordinates
no.uses_sf_prj <- lapply(no.uses_sf_eez, 
                         st_transform, 
                         '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84')

# Create list of points and lines to buffer
pts_lines <- no.uses_sf_prj[c('cables_active_csiro',
                              'updatedpetroleumpipelines_AUS_aodn',
                              'Ports_terms', 'shipping')]

# # Buffer function
# buffer_1km <- function(x) {
#   st_buffer(x, dist = units::set_units(1, km))
# }

# Create 1 Km buffer
pts_lines <- lapply(pts_lines, buffer_1km)

# Remove original points and lines from list
no.uses_sf_prj_polys <- no.uses_sf_prj[-c(1,4,5,9)]

# Add buffered points and lines to list
no.uses_sf_prj_all <- c(no.uses_sf_prj_polys, pts_lines)

# Make the names 'prettier'
names(no.uses_sf_prj_all) <- c('Oil lease areas', 'Recreational boat use', 
                               'Wind farm areas', 'Commercial fishing',
                               'WindFarm_WA', 'Recreational parks',
                               'MPAs', 'IPAs', 'Aquaculture',
                               'Underwater cables', 'Petroleum pipelines',
                               'Ports and terminals', 'Shipping')

# Add layer name to each dataframe
no.uses_sf_prj_all <- no.uses_sf_prj_all %>%  
  map2(names(no.uses_sf_prj_all), ~mutate(.x, layer = .y))


### Dissolve polygons

## Geometry names do not match
## Will do them separately

# Make geoms valid
no.uses_sf_prj_all <- lapply(no.uses_sf_prj_all, st_make_valid)

# Save list
saveRDS(no.uses_sf_prj_all, here('output_data', 'ocean_uses', 'no_uses_sf_prj_all'))


## Rename geometry columns

# Create function
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}

# Rename
no.uses_geoms <- lapply(no.uses_sf_prj_all, rename_geometry, 'geometry')

# Get layer column with geom
uses_area_geoms <- bind_rows(lapply(
  no.uses_geoms, "[", 'layer'))

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

# Save
st_write(uses_area_diss, here('output_data', 'ocean_uses', 'uses_area_diss.gpkg'))


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

# Save
st_write(uses_area_final, here('output_data', 'ocean_uses', 'uses_area_final.gpkg'))

# Barplot with industries on x axis and area on y axis
area_bp_2 <- 
  ggplot(uses_area_final) +
  geom_bar(aes(x = layer, y = area_km2/1000000), 
           stat = 'identity', col = 'black', fill = 'green4') +
  labs(x = '', y = expression('Total area' ~ '(mil' ~ Km^2*')')) +
  theme_classic() +
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_y_continuous(labels = comma)
area_bp_2

# Save
ggsave(here('output_data', 'ocean_uses', 'area_bp_2.png'), area_bp_2,
       width = 8, height = 5)
