rm(list=ls())
library(raster)
library(rgdal)
library(maptools)

computer <- "//654cw-20990/Amy"
LH.zonation.dir <- paste0(computer,"/GIS_data/Hunter/zonation/corridor/") 

# import region mask for LHSA
LH.mask <- raster(paste0(computer,"/GIS_data/Hunter/mask files/LH.mask.tif"))
LH.mask[!is.na(LH.mask)] <- 0

# LHSA urban clipping mask
LH.urban.clipping.mask <- raster(paste0(computer,"/GIS_data/Hunter/mask files/LH.urban.clipping.mask.tif"))
LH.urban.clipping.mask[!is.na(LH.urban.clipping.mask)] <- 0

WoodyVeg100.mask <- raster(paste0(computer,"/GIS_data/Hunter/mask files/LH.woody.mask.100m.tif"))
WoodyVeg100.mask[!is.na(WoodyVeg100.mask)] <- 0

WoodyVeg25.mask <- raster(paste0(computer,"/GIS_data/Hunter/mask files/LH.woody.mask.25m.tif"))
WoodyVeg25.mask[!is.na(WoodyVeg25.mask)] <- 0

gap.resistance <- raster(paste0(LH.zonation.dir,"LH_GAP_CLoSR_OEH/luse_4fin.tif"))

# read in a shape and mask file file
# mask <- raster('C:/Users/hkujala/work/Mining offsets/LH.urban.clipping.mask.tif')
# 
# # creating a corridor domain layer from LU resistance + gap distance
# # read files
# gap.resistance <- raster('C:/Users/hkujala/work/Corridor work/LH_GAP_CLoSR_OEH/luse_4fin.tif')
# woody.patches <- readShapePoly('C:/Users/hkujala/work/Corridor work/LH_GAP_CLoSR_OEH/Patch.shp')

# remove dispersal barriers
par(mfrow=c(1,1))
plot(gap.resistance)
gap.resistance[which(gap.resistance[]==65535)] <- 125

# remove existing woody patches
plot(gap.resistance)

# gap.res.project <- projectRaster(gap.resistance, WoodyVeg.mask, method='ngb')
gap.res.matrix <- mask(gap.resistance, WoodyVeg25.mask, inverse=T)
plot(gap.res.matrix)

# invert the resistance values to create an increasing weight 
inv.gap.res.matrix <- 126-gap.res.matrix
plot(inv.gap.res.matrix)

# aggregate resolution from 25m to 100m
inv.gap.res.matrix.100 <- aggregate(inv.gap.res.matrix, fact=4, fun='mean')
plot(inv.gap.res.matrix.100)

# project and snap to match all other layers
inv.gap.res.matrix.100.proj <- projectRaster(inv.gap.res.matrix.100, LH.urban.clipping.mask, method='ngb')
plot(inv.gap.res.matrix.100.proj)

# clip to cut out buffers and built-up areas and woody patches at the 100m resolution scale
inv.gap.res.matrix.100.proj.clipped <- mask(mask(inv.gap.res.matrix.100.proj, LH.urban.clipping.mask),WoodyVeg100.mask,inverse=T)
plot(inv.gap.res.matrix.100.proj.clipped)

# save the layer
writeRaster(inv.gap.res.matrix.100.proj.clipped, paste0(LH.zonation.dir,'resistanceNA.tif'), overwrite=T)

# set the woody patch values to zero
inv.gap.res.matrix.100.proj.clipped.patch0 <- inv.gap.res.matrix.100.proj.clipped
inv.gap.res.matrix.100.proj.clipped.patch0[is.na(inv.gap.res.matrix.100.proj.clipped.patch0)] <- 0
inv.gap.res.matrix.100.proj.clipped.patch0 <- mask(inv.gap.res.matrix.100.proj.clipped.patch0, LH.urban.clipping.mask)

writeRaster(inv.gap.res.matrix.100.proj.clipped.patch0,paste0(LH.zonation.dir,"resistance0.tif"),overwrite=T)

inv.gap.res.matrix.100.proj.clipped.patch101 <- inv.gap.res.matrix.100.proj.clipped.patch0
inv.gap.res.matrix.100.proj.clipped.patch101[inv.gap.res.matrix.100.proj.clipped.patch0 == 0] <- 101
writeRaster(inv.gap.res.matrix.100.proj.clipped.patch101,paste0(LH.zonation.dir,"resistance.tif"),overwrite=T)
