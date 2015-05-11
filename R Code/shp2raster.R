shp2raster <- function(shp, mask.raster, label, value, bkg = 0,transform = FALSE, proj.from = NA, proj.to = NA, map = TRUE, save=TRUE,mask=TRUE) {
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    if(is.na(proj4string(shp))) proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background raster with 
  # 'value' as the areas where the shapefile are and 'bkg' representing the areas outside the polygon
  r <- rasterize(shp, mask.raster,field=value,background=bkg)
    # mask out areas outside the boundary of the mask raster
    if(mask==T) r <- mask(r,mask.raster)
  
  # save a copy of the raster as a tif in the working directory
  if(save==TRUE) writeRaster(r, filename = label, format = "GTiff",overwrite = T)
    
  # plot map of new raster
  if (map == TRUE) plot(r, main = label, axes = F, box = F)
  
  names(r) <- label
  return(r)
}