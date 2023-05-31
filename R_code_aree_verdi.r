# libraries
require(raster)
require(RStoolbox)
require(ggplot2) 
require(gridExtra)
require(rasterdiv)
require(lattice)
require(rasterVis)
require(latticeExtra)
require(sf)

# rasters
img <- brick("C:/aree_verdi/sentinel2/raster/prova_nuova/sentinel2__comunebologna_resampledbands10m.tif")
fvc <- brick("C:/aree_verdi/sentinel2/raster/prova_nuova/comune_bologna__FVC10m.tif")
lai <- brick("C:/aree_verdi/sentinel2/raster/prova_nuova/comune_bologna__LAI10m.tif")

fvc2 <- fvc$comune_bologna__FVC10m_2 # layer indice
lai2 <- lai$comune_bologna__LAI10m_2 # layer indice

# shapefiles
bologna <- st_read("C:/aree_verdi/vettoriali/comune_bologna.shp")
aree_verdi_geop <- st_read("C:/aree_verdi/vettoriali/aree_verdi_comune/V_PSR_GPG.shp")

# vegetation indices
ndvi <- (img$sentinel2__comunebologna_resampledbands10m_8 - img$sentinel2__comunebologna_resampledbands10m_4)/(img$sentinel2__comunebologna_resampledbands10m_8 + img$sentinel2__comunebologna_resampledbands10m_4)
gci <- (img$sentinel2__comunebologna_resampledbands10m_8) / (img$sentinel2__comunebologna_resampledbands10m_3 - 1)
gndvi <- (img$sentinel2__comunebologna_resampledbands10m_8 - img$sentinel2__comunebologna_resampledbands10m_3) / (img$sentinel2__comunebologna_resampledbands10m_8 + img$sentinel2__comunebologna_resampledbands10m_3)
ndmi <- (img$sentinel2__comunebologna_resampledbands10m_8 - img$sentinel2__comunebologna_resampledbands10m_12) / (img$sentinel2__comunebologna_resampledbands10m_8 + img$sentinel2__comunebologna_resampledbands10m_12)

# masks
ndvi_mask <- mask(ndvi, bologna)
gci_mask <- mask(gci, bologna)
gndvi_mask <- mask(gndvi, bologna)
ndmi_mask <- mask(ndmi, bologna)
fvc_mask <- mask(fvc2, bologna)
lai_mask <- mask(lai2, bologna)
ndvi_areeverdi <- mask(ndvi, aree_verdi_geop) # ndvi aree verdi croppato da shpfile del geoportale

# vegetation mask
# 0.2 la vegetazione è sovrastimata
soglia3 <- 0.3
soglia4 <- 0.4
soglia5 <- 0.5

mask_veg <- ndvi >= soglia5

mask_veg[mask_veg == TRUE] <- 1
mask_veg[mask_veg == FALSE] <- 0

# palettes per plot vari
cl_ndvi <- colorRampPalette(c("red", "yellow", "lime green", "dark green"))(500)
cl_green <- colorRampPalette(c("white", "light green", "dark green"))(500)
cl_ndmi <- colorRampPalette(c("red", "yellow", "cyan", "blue"))(500)
cl_th <- colorRampPalette(c("white", "black"))(500)

# par degli indici veg
par(mfrow = c(2,3))
plot(ndvi, col = cl_ndvi, main = 'NDVI')
plot(ndmi, col = cl_ndmi, main = 'NDMI')
plot(gndvi, col = cl_green, main = 'GNDVI')
plot(gci, col = cl_green, main = 'GCI')
plot(fvc$sentinel2_FVC10m_1, col = cl_green, main = 'FVC')
plot(lai$sentinel2_LAI10m_1, col = cl_green, main = 'LAI')

# par fci e nci
nci <- stack(img$sentinel2__comunebologna_resampledbands10m_4,
             img$sentinel2__comunebologna_resampledbands10m_3,
             img$sentinel2__comunebologna_resampledbands10m_2)

fci <- stack(img$sentinel2__comunebologna_resampledbands10m_8,
             img$sentinel2__comunebologna_resampledbands10m_4,
             img$sentinel2__comunebologna_resampledbands10m_3)

nci_mask <- mask(nci, bologna)
fci_mask <- mask(fci, bologna)

par(mfrow = c(1,2))
plotRGB(nci_mask, axes = TRUE, stretch = 'lin', main = 'Natural color image')
plotRGB(fci_mask, axes = TRUE, stretch = 'lin', main = 'False color image')

# NDVI Rao         
rao_ndvi <- paRao( 
  x = ndvi_areeverdi, 
  area = NULL, 
  field = NULL, 
  dist_m = "euclidean", 
  window = 3, 
  alpha = 1, 
  method = "classic", 
  rasterOut = TRUE, 
  lambda = 0, 
  na.tolerance = 1.0, 
  rescale = FALSE, 
  diag = TRUE, 
  simplify = 1, 
  np = 1, 
  cluster.type = "SOCK", 
  debugging = FALSE
)


rao_ndvi_rast <- rao_ndvi$window.3$alpha.1
# plot(rao_ndvi_rast)

# standard deviation
ndvi_sd <- focal(ndvi, w = matrix(1/9, nrow = 3, ncol = 3), fun = sd)

col_rao <- colorRampPalette(c("purple", "turquoise", "lime green", "yellow", "magenta"))(500)
plot(rao_ndvi_rast, col = col_rao, main = 'Rao Index')

# write raster
# writeRaster(raster, filename = "/path", format = "GTiff")
# writeRaster(fvc_th, filename = "C:/aree_verdi/imgs/raster_out_r/fvc_th_05", format = 'GTiff')


