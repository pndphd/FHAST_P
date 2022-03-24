##### Description #####
# This runs the scripts and functions to sampel shape filees witht he grid

##### Inputs #####
# What resolution is the grid you are using
resolution = 200
# what is the max buffer distance of the grid
max_buffer = 500
# Do you want the plots to show up
show_plots = 1
# select the number of computer cores to use in analysis
number_cores = 12
# Location of grid
grid_folder = "../../../temporary/R/"
# Location of the shape folder
shape_folder = "../../../data/GIS/example_shapes_1/"
# Where to save the output
output_folder = "../../../temporary/NetLogo/"

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
# Library to parallelize purrr
library(furrr)
# Library to join graphs
library(patchwork)

# Make sure dplyr select is the default
select = dplyr::select

# Setup furrr
future::plan(multisession, workers = number_cores)
future.seed = TRUE

# Load the function to sample shapes on the grid
source("./scripts/Sample_Shapes_Functions.R")

##### Functions #####
# Make a function 
# functionName = function(input1){
#   return(output)
# }

##### Load Files #####
# load the river grid
river_grid = readRDS(paste0(grid_folder, "river_grid_", resolution, "_", max_buffer, ".rds"))
# Load the cover file
cover_shape = st_read(paste0(shape_folder, "Cover_Shape.shp"), quiet = TRUE) 
# Load the zone file
zone_shape = st_read(paste0(shape_folder, "River_Zones.shp"), quiet = TRUE) 
# Load the river bank file
riverbank_shape = st_read(paste0(shape_folder, "River_Lines.shp"), quiet = TRUE) 

##### Pre Processing #####
# Do any basic pre processing here

##### Main Work #####
# make a list of files san variabel names 
variables = list("Cover", "Zones", "Value")
shape_files = list(cover_shape, zone_shape, riverbank_shape)

# Sample all the shapes over the grid
sampeled_shapes = sample_all_shapes(river_grid, shape_files, variables)

shapes_csv = sampeled_to_csv(sampeled_shapes)

##### Save Outputs #####
write.csv(shapes_csv, paste0(output_folder, "Shape_Data_Input_", resolution,"_", max_buffer, ".csv"), na = "-9999")


  
