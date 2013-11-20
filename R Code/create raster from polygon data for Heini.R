library(raster)
library(rgdal)

create.raster <- function (s, mask.raster, label, value, transform=TRUE) {
  
  if(transform==TRUE) {
    proj4string(s) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    s <- spTransform(s, CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  }
  
  r <- rasterize(s,mask.raster)
  r[!is.na(r)] <- value
  r <- mask(merge(r,mask.raster),mask.raster, filename=paste0(output.directory,label), format="GTiff", overwrite=T)
  names(r) <- label
  
  plot(r, main=label)
  
  return(r)
}

## example

# read in a shape file
dop.eia1 <- readShapePoly(paste0(envi.directory,"DOP/DoP_EIA1_20130809.shp"))

# create a raster based on polygons with the Land_Type "Developed Urban" or "Existing Industrial" where the background values == 0 and the bits you care about have a value of 1. The polygons are merged into the mask raster that represents the Perth area and the raster is saved in the working directory as a GTiff using the name provided as label.  In this case the shapefile and the mask raster are in the same projection, so you don't need to transform the data first.

dop.eia1.existing <- create.raster(dop.eia1[dop.eia1$Land_Type=="Developed Urban" | dop.eia1$Land_Type=="Existing Industrial",],perth, label="DOP_EIA1_existing", value=1, transform=FALSE)