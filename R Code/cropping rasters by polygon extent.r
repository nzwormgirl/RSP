rm(list=ls())
library(raster)
library(rgdal)
library(maptools)


setwd(gis.directory <- "C:/Users/awhitehead/Documents/GIS data/Hunter")

# open hunter region raster file
  hunter.raster <- readGDAL(paste0(gis.directory,"/data from Brendan/data/region/regiongrid.asc"))
    hunter.raster <- raster(hunter.raster)
    # set all cells to zero to make PA mask
    hunter.raster[!is.na(hunter.raster)] <- 0

# open urban lands shapefile
  urban.shapefile <- readShapePoly(paste0(gis.directory,"/Plans/LHRS_2006_proposed_urban_lands/lh_proposed_urban_lands.shp"))

# plot to make sure they actually overlap
  plot(hunter.raster)
  plot(urban.shapefile, add=TRUE)
  plot(extent(urban.shapefile), add=T) # plot the extent of the shapefile

# crop hunter raster by extent of urban lands shapefile (I think this is what you wanted?!)
  crop.raster <- crop(hunter.raster,extent(urban.shapefile))
  plot(crop.raster)
  writeRaster(crop.raster,"myCroppedRaster.asc")

# create a cropped presence-absence raster of urban lands  
  urban.raster <- rasterize(urban.shapefile,hunter.raster,field=1)
    urban.raster <- merge(urban.raster,hunter.raster)
    urban.raster <- crop(urban.raster,extent(urban.shapefile))
    plot(urban.raster)
  writeRaster(urban.raster,"UrbanLandsCroppedRaster.asc")



  


                          
                          