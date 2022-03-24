# This folder contains the functions for Sample_Raster_Run.R

##### get_type_letter #####
# this function translates the yype word into the letter
get_type_letter = function(type = NULL){
  if(type == "depth"){
    type_letter = "D"
  } else if (type == "velocity"){
    type_letter = "V"
  } else {
    stop("Invalid type of raster. Choose 'depth' or 'velocity'.")
  }
  
  return(type_letter)
}

##### load_rasters #####
# This function loads and reprojects the rasters
load_rasters = function(type = NULL,
                        folder = NULL,
                        flows = NULL,
                        EPGS = NULL){

  type_letter = get_type_letter(type)
  
  stack = stack(map(flows,~raster(paste0(folder, "/", type_letter, .x, ".tif"))))
  
  if(type_letter == "D"){
    # Use terrain to calculate the slope abd the correction for area
    slope_raster = raster(paste0(folder, "D", max(flows), ".tif")) %>% 
      terrain(opt="slope") %>% 
      calc(fun = function(x){1/cos(x)})
    # Give the valuse a name
    names(slope_raster) = "correction_factor"
    
    stack = stack %>% 
      stack(slope_raster)
    
  } 

  stack_prj = stack %>% 
    projectRaster(crs = crs(paste0("+init=epsg:", EPSG)))
  
  return(stack_prj)
}

##### sample_grid #####
# This function samplees the raster stack using the river grid
# if depth is selected it also gets the bottom area
sample_grid = function(stack = NULL,
                       grid = NULL,
                       type = NULL){
  
  type_letter = get_type_letter(type)

  samples = stack %>% 
    exact_extract(grid, 'mean', progress = FALSE) %>%  
    data.frame() %>% 
    # bind it back to the polygons
    cbind(data.frame(grid)) %>% 
    # set left and right bank correctly
    mutate(lat_dist = ifelse(lat_dist>0,ifelse(left_or_right>0, lat_dist, -lat_dist),0)) %>% 
    # Convert to simple feature then a df and remove uncessary rows
    data.frame() %>% 
    dplyr::select(all_of(paste0("mean.D", flows)),
                  lat_dist,
                  # if velocity the correction factor is not inside 
                  contains("mean.correction_factor"),
                  distance,
                  area) %>% 
    dplyr::rename(x=lat_dist, y = distance) 
  
  if(type_letter == "D"){
    samples = samples %>% 
      mutate(bottom_area = ifelse(is.na(mean.correction_factor), area, area*(mean.correction_factor)))
  }
  
  return(samples)
}