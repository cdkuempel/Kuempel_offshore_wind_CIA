
#----Matrix showing the number of industry overlap occurrences----#


library(tidyverse)
library(here)
library(sf)
library(janitor)
library(fasterize)
#devtools::install_github("hypertidy/tabularaster")
library(tabularaster)
library(stringr)
library(pbmcapply)
library(raster)

sf_use_s2(F)

# Should we be using an Australian specific equal area projection?

# Import project ocean uses data
no_uses_sf_prj_all <- readRDS(here('output_data/ocean_uses/no_uses_sf_prj_all'))

# Project
list_sf_trans <- lapply(no_uses_sf_prj_all, function(x) st_transform(x, "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs
"))

# Make geoms valid
list_sf_trans2 <- lapply(list_sf_trans, function(x) st_make_valid(x))

# Import EEZ boundary
eez_boundary <- st_read(here('output_data/ocean_uses/eez_boundary.gpkg'))

# Project eez
eez_boundary <- st_transform(eez_boundary, "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs
") %>% 
  st_make_valid(.)

# Crop all to eez
no_uses_sf_prj_crop <- lapply(list_sf_trans2, st_crop, eez_boundary)


## Rasterize and create dataframes

# Create template raster grid
temp_rast<-raster(eez_boundary, res = 1000) #determine resolution to use

#x<-no_uses_sf_prj_crop[[1]]

# Create ocean uses rasters and dataframes
rast_uses<-function(x){
  use_rast<-fasterize(x, temp_rast, field = NULL, fun = "last", background = NA)
  use_df<-as_tibble(use_rast, xy = TRUE)
  name<-str_replace_all(unique(x$layer), pattern = " ", repl = "")
  names(use_df)[1] <- name
  writeRaster(use_rast, paste0(here("output_data", 'ocean_uses'), name, "_ras.tif"), overwrite = T)
  write_csv(use_df, paste0(here("output_data", "ocean_uses"), name, "_df.csv"))
}
pbmclapply(no_uses_sf_prj_crop, rast_uses, mc.cores = length(no_uses_sf_prj_crop), mc.style = "ETA")

# Read in dataframes and group together
use_file_list<-list.files(here(""))


## Loop through the ocean uses list and generate 
## a matrix with number of overlap occurrences

# Length of list
n <- length(no_uses_sf_prj_crop)

# Get all combos of list elements and exclude 1-1, 2-2, 3-3 etc.
combodf <- expand.grid(x = 1:n, y = 1:n) %>% 
  filter(x != y)

# Create list to store output
tmp <- list()


### Computer performance may require running this in chunks and combine later

# Loop to find intersects
 for(i in 1:nrow(combodf)){
   combo <- combodf[i,] # Create index (where will the loop look for values?)
   first <- no_uses_sf_prj_crop[[combo$x]] # Indicate where to find first industry
   second <- no_uses_sf_prj_crop[[combo$y]] # Indicate where to find second industry
   int <- st_overlaps(first, second, sparse = F) # Check for overlap = TRUE
   # Sum elements where overlap = TRUE - WHAT DOES ELEMENTS MEAN?
   count <- sum(int, na.rm=TRUE)
   # Store output
   tmp[[i]] <- count
 }

# Convert results to numeric vector with number of overlaps
results <- do.call(c, tmp)

# Add results to dataframe of combinations
combodf$num_overlaps <- results

# # Save
# write_csv(combodf, here('output_data/ocean_uses/combodf.csv'))


### Following is for Bass Strait and may need to be changed for Australia

# Create columns with industry names
combodf <- combodf %>%
  mutate(industry_x = case_when(x == 1 ~ 'Cables',
                                x == 2 ~ 'Oil lease areas',
                                x == 3 ~ 'Petroleum pipelines',
                                x == 4 ~ 'Ports/terminals',
                                x == 5 ~ 'Fisheries',
                                x == 6 ~ 'Rec. parks',
                                x == 7 ~ 'MPAs',
                                x == 8 ~ 'IPAs',
                                x == 9 ~ 'Aquaculture',
                                x == 10 ~ 'Wind farm areas',
                                x == 11 ~ 'Shipping')) %>%
  mutate(industry_y = case_when(y == 1 ~ 'Cables',
                                y == 2 ~ 'Oil lease areas',
                                y == 3 ~ 'Petroleum pipelines',
                                y == 4 ~ 'Ports/terminals',
                                y == 5 ~ 'Fisheries',
                                y == 6 ~ 'Rec. parks',
                                y == 7 ~ 'MPAs',
                                y == 8 ~ 'IPAs',
                                y == 9 ~ 'Aquaculture',
                                y == 10 ~ 'Wind farm areas',
                                y == 11 ~ 'Shipping'))

# Convert results dataframe into a matrix
matrix <- ftable(xtabs(num_overlaps ~ industry_x + industry_y, data = combodf))

# Format table
matrix_table <- stats:::format.ftable(matrix, quote = FALSE)

# Remove second row and column (blank)
matrix_table <- as.data.frame(matrix_table[-2 ,-2])

# Convert first row to column names
matrix_table <- matrix_table %>%
  row_to_names(row_number = 1)

# Save
write_csv(matrix_table, here('output_data/ocean_uses/industry_overlap_AUS.csv'))
