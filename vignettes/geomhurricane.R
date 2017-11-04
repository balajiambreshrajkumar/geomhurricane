## ---- eval=FALSE---------------------------------------------------------
#  # Imports
#  library(dplyr)
#  library(iterators)
#  library(readr)
#  library(geosphere)
#  library(ggplot2)
#  library(ggmap)
#  library(geomhurricane)
#  
#  ike_data <- read_hurricane_data(paste0(getwd(), "/ebtrk_atlc_1988_2015.txt"), "IKE-2008", "2008-09-13 06:00:00")
#  
#  custom_map5 <- get_map("Louisiana", zoom = 5, maptype = "toner-background") %>% ggmap(extent = "device")
#  hurricane_ike <- custom_map5 +
#    geom_hurricane(data = ike_data,
#                   aes(x = longitude, y = latitude,
#                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#                       fill = wind_speed, color = wind_speed)) +
#    scale_color_manual(name = "Wind speed (kts)",
#                       values = c("red", "orange", "yellow")) +
#    scale_fill_manual(name = "Wind speed (kts)",
#                      values = c("red", "orange", "yellow"))
#  # Save the data
#  jpeg("hurricane_data.jpeg")
#  plot(hurricane_ike)
#  dev.off()

