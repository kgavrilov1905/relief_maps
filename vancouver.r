
library(raster)
library(sp)
library(rayshader)
library(tidyverse)
localtif = raster::raster("DEM_BC.tif")

elmat = raster_to_matrix(localtif)
elmat_red = elmat[1000:5000, 3500:5996] # Reducing the matrix
elmat_exp = elmat[1300:4000, 3700:5500]

elmat_exp %>% sphere_shade(texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#FFE5CC")) %>%
  add_water(detect_water(elmat_exp, cutoff = 0.89444458), color = "desert") %>%
  plot_map()

elmat_exp %>% sphere_shade(texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#FFE5CC")) %>%
  add_water(detect_water(elmat_exp, cutoff = 0.89444458), color = "lightblue") %>%
  plot_3d(elmat_exp, zscale = 10, theta = -45, zoom = 0.5, phi = 25, windowsize = c(1000, 800))

render_depth(focus = 0.6, focallength = 200, clear = TRUE) # depth

render_label(elmat_exp, x = 900, y = 700, z = 1000, zscale = 10, text = "Vancouver",
             dashed = TRUE, textsize = 1, linewidth = 5, linecolor = "red")
