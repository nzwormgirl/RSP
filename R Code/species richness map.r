packages(raster)
packages(maptools)
packages(RColorBrewer)
setwd("~/GIS_data/Hunter/zonation/greater hunter/")

sdms <- dir(pattern=".tif$")

sdm_stack <- lapply(sdms,raster)


calc_sum <- function (sdm_stack, lower, upper) {
  sdm_stack_subset <- stack(sdm_stack[lower:upper])
  
  
  return(calc(sdm_stack_subset,sum))
}

sp_index <- calc_sum(sdm_stack,1,100)
sp_index1 <- calc_sum(sdm_stack,101,200)
sp_index2 <- calc_sum(sdm_stack,201,300)
sp_index3 <- calc_sum(sdm_stack,301,400)
sp_index4 <- calc_sum(sdm_stack,401,500)
sp_index5 <- calc_sum(sdm_stack,501,600)
sp_index6 <- calc_sum(sdm_stack,601,700)
sp_index7 <- calc_sum(sdm_stack,701,800)
sp_index8 <- calc_sum(sdm_stack,801,900)
sp_index9 <- calc_sum(sdm_stack,901,1000)
sp_index10 <- calc_sum(sdm_stack,1001,1115)

all_index <- stack(sp_index,sp_index1,sp_index2,sp_index3,sp_index4,sp_index5,sp_index6,sp_index7,sp_index8,sp_index9,sp_index10)

final_index <- calc(all_index,sum)
resampled_final_index <- final_index/max(final_index,na.rm=T)
plot(final_index)

LH <- readShapePoly("~/GIS_data/Hunter/All data from harddrive/From DO/OEH_Lower_Hunter_18122012/Administrative/LHRS_Study_Area.shp")

LH_final_index <- crop(mask(final_index,LH),LH)
richness.colour <- brewer.pal(9,"Reds")
png("~/RSP/richness.png",height=10,width=10,units="cm",res=300)
plot(LH_final_index, axes=F, box=F, legend=F, col=richness.colour)
plot(LH,add=T,border="grey")
dev.off()
writeRaster(LH_final_index, "LH_species_richness_index.tif",overwrite=T)