
#elevation1 = raster("vancouver_tif/N48W124.hgt")
elevation2 = raster("vancouver_tif/N49W124.hgt")
#elevation3 = raster("vancouver_tif/N48W123.hgt")
elevation4 = raster("vancouver_tif/N49W123.hgt")

van_elev = raster::merge(elevation2, elevation4)

height_shade(raster_to_matrix(van_elev)) %>% plot_map()

van_r = raster("vancouver_tif/LC08_L1TP_047026_20200814_20200822_01_T1_B4.TIF")
van_g = raster("vancouver_tif/LC08_L1TP_047026_20200814_20200822_01_T1_B3.TIF")
van_b = raster("vancouver_tif/LC08_L1TP_047026_20200814_20200822_01_T1_B2.TIF")

van_rgb = raster::stack(van_r, van_g, van_b)
plotRGB(van_rgb, scale = 255^2)

van_rgb_corrected = sqrt(stack(van_r, van_g, van_b))
plotRGB(van_rgb_corrected)

crs(van_r)
crs(van_elev)

van_elevation_utm = projectRaster(van_elev, crs = crs(van_r), method = "bilinear")
crs(van_elevation_utm)

# Resizing
bottom_left = c(y = -123.44, x = 49.00)
top_right = c(y = -122.304, x = 49.607)

extent_latlong = sp::SpatialPoints(rbind(bottom_left, top_right), proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
exten_utm = sp::spTransform(extent_latlong, raster::crs(van_elevation_utm))

van_rgb_cropped = crop(van_rgb_corrected, extent(exten_utm))
plotRGB(van_rgb_cropped)
elevation_cropped = crop(van_elevation_utm, extent(exten_utm))

van_rgb_contrast_stretch = raster::stretch(van_rgb_cropped, minq = 0.02,maxq = 0.98)
raster::plotRGB(van_rgb_contrast_stretch)

names(van_rgb_contrast_stretch) = c("r", "g", "b")

van_r_cropped = raster_to_matrix(van_rgb_contrast_stretch$r)
van_g_cropped = raster_to_matrix(van_rgb_contrast_stretch$g)
van_b_cropped = raster_to_matrix(van_rgb_contrast_stretch$b)

van_elmat = raster_to_matrix(elevation_cropped)

van_rgb_array = array(0, dim = c(nrow(van_r_cropped), ncol(van_r_cropped), 3))

van_rgb_array[,,1] = van_r_cropped/255
van_rgb_array[,,2] = van_g_cropped/255
van_rgb_array[,,3] = van_b_cropped/255

van_rgb_array = aperm(van_rgb_array, c(2, 1, 3))

plot_map(van_rgb_array)

van_rgb_final = rescale(van_rgb_contrast_stretch, to = c(0,1))
plot_map(van_rgb_contrast)

plot_3d(van_rgb_array, van_elmat, windowsize = c(1100,900), zscale = 15, zoom = 0.5)



