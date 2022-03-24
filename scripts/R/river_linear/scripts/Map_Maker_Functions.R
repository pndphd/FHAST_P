# Function ----------------------------------------------------------------
# This is a function to make a leaflet map

make_leaflet_map = function(shape = NULL,
                            type = NULL){

  if(type == "point"){
    plot = leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = F)) %>%
      addCircleMarkers(data = (shape %>% st_transform(4326))) 
  } else if (type == "line"){
    plot = leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = F)) %>%
      addPolylines(data = (shape %>% st_transform(4326)))
  } else if (type == "poly"){
    plot = leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = F)) %>%
      addPolygons(data = (shape %>% st_transform(4326)))
  } else{
    stop("Invalid Selection of type. Chose 'point', 'line', or 'poly'.")
  }

  # print(plot)
  return(plot)
}