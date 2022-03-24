##### Description #####
# Enter description here 

##### Inputs #####

##### Libraries #####
library(tidyverse)
library(lubridate)
# allows combining of plots
library(patchwork)
# for working with rasters
library(raster)
# for working with shape files
library(sf)
# Open and write excel files
library(openxlsx)
# The color palet library
library(viridis)

##### Functions #####
# Make a function 
# functionName = function(input1){
#   return(output)
# }

##### Load Files #####
csv_data = read.csv(file = "cover_simulation_data.csv") %>% 
  mutate(inv_mean_dis_w_0 = 1/mean_dis_w_0,
         mfp = 1/(sqrt(max_area)*num_patches)) 
  # filter(num_patches == 4, max_area >0.1)

dist_area_plot = ggplot(data = csv_data, aes(x = (pct_cover),
                                             y = (mean_dis_no_0))) +
  theme_classic(base_size = 20) +
  geom_point()+
  labs(x = "%", y = "Dist")
windows(xpos = 2000)
print(dist_area_plot)
                          

##### Pre Processing #####
# Do any basic pre processing here

##### Main Work #####
##### Main Thing 1 #####
# Do the work for the first part

##### Main Thing 2 #####
# Do the work for the second part

##### Plots #####
# Make a window on a second screen
# windows(xpos = 2000)
# 
# plotName = ggplot(data, aes(x = x, y = y))+
#   theme_classic(base_size = 30) +
#   labs(y = "y label", x = "x label") +
#   theme(legend.title = element_blank(),
#     legend.position = c(0.8, 0.2)) +
#   geom_path(aes(x = x1, y = y1, color = "definedColor")) +
#   scale_color_manual(name = NULL, values = c(definedColor = "gray20", definedColor2 = "dodgerblue3")) +
#   scale_color_brewer(palette = "Paired") +
#   scale_color_manual(values = cbPalette) +
#   geom_text(aes(y=interval, label = count), vjust=1.6, 
#             color="Black", size=5, position = position_dodge(width = 0.9)) +
#   geom_hline(yintercept = 0, size = 1, color = "black") 
# print(plotName)

##### Save Outputs #####
# ggsave(filename = "path",
#        plotName,
#        dpi = 300,
#        device = "png",
#        height = 8,
#        width = 8,
#        units = "in") 
# 
# write.xlsx(x,
#            file = "path",
#            sheetName = "Sheet Name")
# 
# write.csv(x,
#           file = "path")
# saveRDS(object,
#         file = "path")