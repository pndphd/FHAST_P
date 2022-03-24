##### Description #####
# This file runs the functions to sample rasters onto a grid 

##### Inputs #####
# Grid file location
grid_file_resolution = 200
# What is the buffer distance of the grid
max_buffer = 500
# Set the desired EPGS code for the GIS files
EPSG = 32610
# Location of rasters
raster_folder = "../../../data/GIS/depth_rasters_large/"
# Location of grid
grid_folder = "../../../temporary/R/"
# Where to save the output
output_folder = "../../../temporary/NetLogo/"
# A vector of the flow values for the input rasters 
flows = c(75, 150, 300, 500, 700, 1000, 1300, 1600, 2000, 3000, 4000)

##### Libraries #####
# the simple features library for most of the shape file stuff
library(sf)
# leaflet for plotting shape files
library(leaflet)
# you know why
library(tidyverse)
# deal with most of the rater calculations
library(raster)
# to smooth the center line of the river
library(smoothr)
# the viridis color map
library(viridis)
# a faster way to do raster sampling with shape files
library(exactextractr)

# Make sure the area function is raster::area 
area = raster::area

# load the functions for this script
source("./scripts/Sample_Raster_Functions.R")

##### Load Files #####
# load the river grid
river_grid = readRDS(paste0(grid_folder, "river_grid_", grid_file_resolution, "_", max_buffer, ".rds"))

##### Main Part #####
# Put all the rasters in a stack
raster_stack = load_rasters(type = "depth",
                            folder = raster_folder,
                            flows = flows,
                            EPGS = EPGS)

# Sample the grid over the raster stack
sampeled_grid = sample_grid(stack = raster_stack,
                           grid = river_grid,
                           type = "depth")

##### Save Outputs #####
# write the data
write.csv(sampeled_grid,
          paste0(output_folder, "Depth_Data_Input_", grid_file_resolution, "_", max_buffer, ".csv"),
          na = "-9999")

##### Make Plots #####
if(0){
  # Plot and example
  stats_plot = sampeled_grid %>% 
    mutate(depth = ifelse(is.nan(mean.D300),NA, mean.D300)) 
  g = ggplot(stats_plot, aes(x = x , y = y , fill = depth )) +
    theme_classic() +
    geom_raster(na.rm = TRUE)
  g
  
  # Plot the bottom difference
  stats_plot = sampeled_grid %>% 
    mutate(difference = ifelse((bottom_area-area) == 0, NA, (bottom_area-area)))
  g = ggplot(stats_plot, aes(x = x , y = y , fill = (difference) )) +
    theme_classic() +
    geom_raster(na.rm = T)
  g
}