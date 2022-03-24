##### Description #####
# This script runs the necessary functions to make and save a rive grid

##### Inputs #####
# Set the desired EPGS code for the GIS files
EPSG = 32610
# Set the resolution you want
# This is how often a point is placed on the river line in m
# and how spaced the buffers are
resolution = 200
# Maximum lateral distance
# what is the max distance away from the center line 
# you want the lateral grid to extend in m
max_buffer = 500
# Do you want the plots to show up
show_plots = 1
# the paths and names of the input files
center_line = "../../../data/GIS/center_line_1/Center_Line.shp"
top_marker = "../../../data/GIS/top_point_1/Top_Point.shp"

##### Libraries #####
# the simple features library for most of the shape file stuff
library(sf)
# leaflet for plotting shapefiles
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

# make sure select is dplyr::select
select = dplyr::select

# load functions used in the script
source("./scripts/Grid_Maker_Functions.R")
source("./scripts/Map_Maker_Functions.R")

##### Load Files #####
shape_files = load_input_files(line = center_line ,
                             top = top_marker,
                             EPGS = EPSG)

##### Pre Processing #####
# get a list of distances for buffers
distances_list = make_distances_list(resolution = resolution,
                                     buffer = max_buffer)

##### Main Work #####
# Make the buffers which are the lateral grid dividers
# This next commented line will filter for only polygons
buffers = make_buffers(distances = distances_list,
                       line = shape_files$line)

# Make a file to tell left from right bank
large_buffer = make_large_buffer(distances = distances_list,
                                 line = shape_files$line)

# Place sample points along the line
sample_points = make_sample_points(resolution = resolution,
                                   line = shape_files$line)

# Make the Voronoi cells
vor_cells = make_vor_cells(points = sample_points,
                           top = shape_files$top,
                           resolution = resolution)

# Combine the buffers and vornoi cells to make the grid
grid = make_grid(resolution = resolution,
                 cells = vor_cells,
                 buffers = buffers,
                 large_buffer = large_buffer)

##### Save Outputs #####
saveRDS(grid, paste0("../../../temporary/R/river_grid_", resolution, "_", max_buffer, ".rds"))
write_sf(grid, paste0("../../../temporary/R/river_grid_", resolution, "_", max_buffer, ".shp"),
         driver ="ESRI Shapefile")


##### make plots #####
make_leaflet_map(grid, "poly")

