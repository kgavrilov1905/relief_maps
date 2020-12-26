
library(RColorBrewer)
bc_dim = data.frame(long = seq(-139.06, -114.03, length.out = 100), lat = seq(48.30, 60, length.out = 100))
prj_dd =  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

bc_elev = get_elev_raster(bc_dim,z = 6, prj_dd)
plot(bc_elev)
bcmat = raster_to_matrix(bc_elev)

bcelev_df = as.data.frame(bc_elev, xy = TRUE)
colnames(bcelev_df)[3] = "z"

# scale_fill_viridis_c(option = "C")
# scale_fill_gradient(low = "#040e82", high = "#005c18")

gs.pal <- colorRampPalette(c("#FFFFFF","#000000"),bias=.1,space="rgb")

bcplot = ggplot(bcelev_df) + geom_raster(aes(x = x, y = y, fill = z)) + scale_fill_viridis_c(option = "D")

plot_gg(bcplot, width = 4, height = 4, scale = 150, multicore = TRUE)
  
  


ggplot() +
  geom_raster(data = DSM_hill_HARV_df,
              aes(x = x, y = y, alpha = HARV_DSMhill)) + 
  scale_alpha(range =  c(0.15, 0.65), guide = "none") + 
  coord_quickmap()

bcmat %>% sphere_shade(texture = create_texture("#fff673","#55967a","#8fb28a","#55967a","#FFE5CC")) %>%
  add_water(detect_water(bcmat, cutoff = 0.89444458), color = "desert") %>%
  plot_map()

bcmat %>% sphere_shade(texture = "desert") %>% 
  add_water(detect_water(bcmat), color = "desert") %>%
  plot_3d(bcmat, zscale = 10)

