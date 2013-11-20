packages(maptools)
packages(raster)
packages(RColorBrewer)

hccrems.raster <- raster("C:/Users/awhitehead/Documents/GIS_Data/Hunter/Maxent_files/ghm_environmental_data/hccrems_mask.asc")
species.data <- read.csv("~/GIS_data/Hunter/Maxent_files/species_data/maxent.data_ALA.NSW.csv")
cma.mask <- readShapePoly("C:/Users/awhitehead/Documents/GIS_data/Hunter/Plans/HCCREMS_AreaOfInterest/HCCREMS_AoI_GDAtm.shp")

png("GH species points.png",height=10,width=10,units="cm",res=300)
plot(hccrems.raster,col="grey", axes=F, legend=F,box="n")
points(species.data$easting,species.data$northing, pch=".")
dev.off()


vegetation <- raster("~/GIS_data/Hunter/Maxent_files/ghm_environmental_data/standardised_rasters/vegetation.asc")
veg.colour <- brewer.pal(9,"Greens")
png("vegetation.png",height=10,width=10,units="cm",res=300)
plot(vegetation, axes=F, legend=F, box="n", col=veg.colour)
dev.off()

mean.rain <- raster("~/GIS_data/Hunter/Maxent_files/ghm_environmental_data/standardised_rasters/mean_rain.asc")
rain.colour <- brewer.pal(9,"Blues")
png("rain.png",height=10,width=10,units="cm",res=300)
plot(mean.rain, col=rain.colour,axes=F, legend=F, box="n")
dev.off()

altitude <- raster("~/GIS_data/Hunter/Maxent_files/ghm_environmental_data/standardised_rasters/altitude.asc")
topo.colour <- brewer.pal(9,"Greys")
png("altitude.png",height=10,width=10,units="cm",res=300)
plot(altitude, col=topo.colour[3:9],axes=F, legend=F, box="n")
dev.off()

fert <- raster("~/GIS_data/Hunter/Maxent_files/ghm_environmental_data/standardised_rasters/fert.asc")
fert.colour <- brewer.pal(5,"YlOrBr")
png("fert.png",height=10,width=10,units="cm",res=300)
plot(fert, col=fert.colour,axes=F, legend=F, box="n")
dev.off()

ninox <- raster("~/GIS_data/Hunter/zonation/greater hunter/Ninox_novaeseelandiae.tif")
ninox.colour <- brewer.pal(9,"Blues")
png("ninox.png",height=10,width=10,units="cm",res=300)
plot(ninox, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
dev.off()

acacia_bynoeana <- raster("~/GIS_data/Hunter/zonation/greater hunter/Acacia_bynoeana.tif")
ninox.colour <- brewer.pal(9,"Greens")
png("~/Teaching/Vegetation Management/Acacia_bynoeana.png",height=10,width=10,units="cm",res=300)
plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
#points(species.data$easting[species.data$species=="Acacia bynoeana"],species.data$northing[species.data$species=="Acacia bynoeana"],pch=".",col="grey")
dev.off()

png("~/Teaching/Vegetation Management/Acacia_bynoeana_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia bynoeana"],species.data$northing[species.data$species=="Acacia bynoeana"])
dev.off()

acacia_pendula <- raster("~/GIS_data/Hunter/zonation/greater hunter/Acacia_pendula.tif")
ninox.colour <- brewer.pal(9,"Greens")
png("~/Teaching/Vegetation Management/Acacia_pendula.png",height=10,width=10,units="cm",res=300)
plot(acacia_pendula, col=ninox.colour,axes=F, legend=T, box="n")
plot(cma.mask,add=T)
#points(species.data$easting[species.data$species=="Acacia pendula"],species.data$northing[species.data$species=="Acacia pendula"],pch=".",col="grey")
dev.off()

png("~/Teaching/Vegetation Management/Acacia_pendula_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia pendula"],species.data$northing[species.data$species=="Acacia pendula"])
dev.off()

acacia_prominens <- raster("~/GIS_data/Hunter/zonation/greater hunter/Acacia_prominens.tif")
ninox.colour <- brewer.pal(9,"Greens")
png("~/Teaching/Vegetation Management/Acacia_prominens.png",height=10,width=10,units="cm",res=300)
plot(acacia_prominens, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
# points(species.data$easting[species.data$species=="Acacia prominens"],species.data$northing[species.data$species=="Acacia prominens"],pch=".",col="grey")
dev.off()

png("~/Teaching/Vegetation Management/Acacia_prominens_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia prominens"],species.data$northing[species.data$species=="Acacia prominens"])
dev.off()

