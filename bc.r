
bc_dim = data.frame(long = seq(-139.06, -114.03, length.out = 100), lat = seq(48.30, 60, length.out = 100))
prj_dd =  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

bc_elev = get_elev_raster(bc_dim,z = 6, prj_dd)
plot(bc_elev)

bcelev_df = as.data.frame(bc_elev, xy = TRUE)
colnames(bcelev_df)[3] = "z"

# ggplot option
bcplot = ggplot(bcelev_df) + geom_raster(aes(x = x, y = y, fill = z)) + scale_fill_viridis_c("Elevation",option = "D") +
  ggtitle("British Columbia Relief Map")
  
plot_gg(bcplot, width = 4, height = 4, scale = 150, multicore = TRUE, zoom = 0.45)
  
bcmat = raster_to_matrix(bc_elev)

bcmat %>% height_shade(texture = hcl.colors(256, "viridis")) %>%
  add_water(detect_water(bcmat, cutoff = 0.89444458), color = "desert") %>%
  plot_map()

bcmat %>% height_shade(texture = hcl.colors(256, "viridis")) %>% 
  add_water(detect_water(bcmat), color = "desert") %>%
  plot_3d(bcmat, zscale = 10, zoom = 0.6)



