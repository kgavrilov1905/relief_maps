
elevation1 = raster("grandcanyon_tif/N36W112.hgt")
elevation2 = raster("grandcanyon_tif/N36W113.hgt")

grand_elev = raster::merge(elevation1, elevation2)

height_shade(raster_to_matrix(grand_elev)) %>% plot_map()

grand_r = raster("grandcanyon_tif/LC08_L1TP_037035_20200925_20201006_01_T1_B4.TIF")
grand_g = raster("grandcanyon_tif/LC08_L1TP_037035_20200925_20201006_01_T1_B3.TIF")
grand_b = raster("grandcanyon_tif/LC08_L1TP_037035_20200925_20201006_01_T1_B2.TIF")

grand_rgb = raster::stack(grand_r, grand_g, grand_b)
plotRGB(grand_rgb, scale = 255^2)

grand_rgb_corrected = sqrt(stack(grand_r, grand_g, grand_b))
plotRGB(grand_rgb_corrected)

crs(grand_r)
crs(grand_elev)

grand_elevation_utm = projectRaster(grand_elev, crs = crs(grand_r), method = "bilinear")
crs(grand_elevation_utm)

# Resizing
bottom_left = c(y = -112.3635, x = 36.00)
top_right = c(y = -111.8138, x = 36.4214)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
exten_utm = sp::spTransform(extent_latlong, raster::crs(grand_elevation_utm))

grand_rgb_cropped = crop(grand_rgb_corrected, extent(exten_utm))
plotRGB(grand_rgb_cropped)
elevation_cropped = crop(grand_elevation_utm, extent(exten_utm))

grand_rgb_contrast_stretch = grand_rgb_cropped

# Adjusting contrast if needed
#grand_rgb_contrast_stretch = raster::stretch(grand_rgb_cropped, minq = 0.15,maxq = 0.98)
#raster::plotRGB(grand_rgb_contrast_stretch)


names(grand_rgb_contrast_stretch) = c("r", "g", "b")

grand_r_cropped = raster_to_matrix(grand_rgb_contrast_stretch$r)
grand_g_cropped = raster_to_matrix(grand_rgb_contrast_stretch$g)
grand_b_cropped = raster_to_matrix(grand_rgb_contrast_stretch$b)

grand_elmat = raster_to_matrix(elevation_cropped)

grand_rgb_array = array(0, dim = c(nrow(grand_r_cropped), ncol(grand_r_cropped), 3))

grand_rgb_array[,,1] = grand_r_cropped/255
grand_rgb_array[,,2] = grand_g_cropped/255
grand_rgb_array[,,3] = grand_b_cropped/255

grand_rgb_array = aperm(grand_rgb_array, c(2, 1, 3))

plot_map(grand_rgb_array)

grand_rgb_contrast = rescale(grand_rgb_array, to = c(0,1))
plot_map(grand_rgb_contrast)

plot_3d(grand_rgb_contrast, grand_elmat, windowsize = c(1100,900), zscale = 15, zoom = 0.5, theta = 45, 
        phi = 30, background = "#e8dcd1", shadowcolor = "#523E2B", fov = 70)
render_snapshot(title_text = "Grand Canyon South Rim, Arizona | By SRTM, 30m DEM",
                title_bar_color = "#1d198c", title_color = "white", title_bar_alpha = 1)


library(rgl)
play3d(spin3d(axis = c(0, 1, 0), rpm = 2), duration = 10)

movie3d(movie = "AnimatedGraph", spin3d(axis = c(0, 1, 0), rpm = 2), duration = 10, dir = "~/Desktop", type = "gif", clean = TRUE)


# Cuting out a chunk for further analysis
bottom_left_red = c(y = -112.0, x = 36.00)
top_right_red = c(y = -111.5538, x = 36.2414)

extent_latlong = sp::SpatialPoints(rbind(bottom_left_red, top_right_red), proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
exten_utm = sp::spTransform(extent_latlong, raster::crs(grand_elevation_utm))

grand_rgb_cropped = crop(grand_rgb_corrected, extent(exten_utm))
plotRGB(grand_rgb_cropped)
elevation_cropped = crop(grand_elevation_utm, extent(exten_utm))

grand_rgb_contrast_stretch = grand_rgb_cropped

names(grand_rgb_contrast_stretch) = c("r", "g", "b")

grand_r_cropped = raster_to_matrix(grand_rgb_contrast_stretch$r)
grand_g_cropped = raster_to_matrix(grand_rgb_contrast_stretch$g)
grand_b_cropped = raster_to_matrix(grand_rgb_contrast_stretch$b)

grand_elmat = raster_to_matrix(elevation_cropped)

grand_rgb_array = array(0, dim = c(nrow(grand_r_cropped), ncol(grand_r_cropped), 3))

grand_rgb_array[,,1] = grand_r_cropped/255
grand_rgb_array[,,2] = grand_g_cropped/255
grand_rgb_array[,,3] = grand_b_cropped/255

grand_rgb_array = aperm(grand_rgb_array, c(2, 1, 3))

plot_map(grand_rgb_array)

grand_rgb_contrast = rescale(grand_rgb_array, to = c(0,1))
plot_map(grand_rgb_contrast)

plot_3d(grand_rgb_contrast, grand_elmat, windowsize = c(1100,900), zscale = 15, zoom = 0.5, theta = 45, 
        phi = 30, background = "#e8dcd1", shadowcolor = "#523E2B", fov = 70)
render_snapshot(title_text = "Grand Canyon South Rim, Arizona | By SRTM, 30m DEM",
                title_bar_color = "#1d198c", title_color = "white", title_bar_alpha = 1)


n_frames = 180
zscale = 15
waterdepthvalues = seq(760, 1820, length.out = 180)
waterdepthvalues = min(grand_elmat)/2 - min(grand_elmat)/2 * cos(seq(0,2*pi,length.out = n_frames))
thetavalues = 135 + 20 * cos(seq(0, 2*pi, length.out = n_frames))

# Elevation ranges from 760.239947573079 to 2623.40985547304
ambmat = ambient_shade(grand_elmat, zscale = zscale)
raymat = ray_shade(grand_elmat, zscale = zscale, lambert = TRUE)
img_frames = paste0("drain", seq_len(n_frames), ".png")


for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  plot_3d(grand_rgb_contrast, grand_elmat, solid = TRUE, zoom = 0.5, shadow = TRUE, zscale = zscale, windowsize = c(1100,900),
          water = TRUE, watercolor = "imhof3", wateralpha = 0.8, waterdepth = waterdepthvalues[i], theta = thetavalues[i],
          phi = 35)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

magick::image_write_gif(magick::image_read(img_frames), 
                        path = "grandcanyon_water.gif", 
                        delay = 6/n_frames)


