packages(maptools)
packages(raster)
packages(RColorBrewer)
library(colorRamps)

# computer <- "I:/Super Computer Data"
computer <- "H:/UM backup"

setwd(paste0(computer,"/GIS_Data/Hunter/"))

GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
GDA94 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

hccrems.raster <- raster(paste0(computer,"/GIS_Data/Hunter/mask files/gh.mask.tif"))
GH.shp <- readShapePoly(paste0(computer,"/GIS_data/Hunter/All data from harddrive/From HCCREMS/Boundaries/AoI/HCCREMS_LGA.shp"),proj4=GDA94)
  GH.shp <- spTransform(GH.shp,GDA94.56)

species.data <- read.csv(paste0(computer,"/GIS_data/Hunter/Maxent_files/species_data/maxent.data_ALA.NSW.csv"))
cma.mask <- readShapePoly(paste0(computer,"/GIS_data/Hunter/Plans/HCCREMS_AreaOfInterest/HCCREMS_AoI_GDAtm.shp"))
lh.mask <- readShapePoly(paste0(computer,"/GIS_data/Hunter/All data from harddrive/From DO/OEH_Lower_Hunter_18122012/Administrative/LHRS_Study_Area.shp"))
gh.pm.mask <- readShapePoly(paste0(computer,"/GIS_data/Hunter/mask files/GH_PM.shp"))
gh.pm.raster <- raster(paste0(computer,"/GIS_Data/Hunter/mask files/GH_PM.mask.tif"))

png("GH species points.png",height=10,width=10,units="cm",res=300,bg="transparent")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(cma.mask,axes=F, legend=F,box="n",col="white")
dev.off()

png("GH_PM boundary.png",height=10,width=10,units="cm",res=300,bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(gh.pm.raster,axes=F, legend=F,box=F,col="whitesmoke")
  plot(gh.pm.mask,add=T)
dev.off()

png("GH boundary.png",height=10,width=10,units="cm",res=300,bg="transparent")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(hccrems.raster,axes=F, legend=F,box=F,col="whitesmoke")
plot(GH.shp,add=T)
dev.off()

png("GH_PM points.png",height=10,width=10,units="cm",res=300)
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(gh.pm.raster,col="whitesmoke", axes=F, legend=F,box=F)
  points(species.data$easting,species.data$northing, pch=".")
  plot(gh.pm.mask,add=T)
dev.off()

png("Aegotheles_cristatus GH_PM points.png",height=10,width=10,units="cm",res=300, bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(gh.pm.raster,col="whitesmoke", axes=F, legend=F,box=F)
  points(species.data$easting[species.data$species=="Aegotheles cristatus"],species.data$northing[species.data$species=="Aegotheles cristatus"], pch=".")
  plot(gh.pm.mask,add=T)
dev.off()

uncertainty <- mask(raster("//654cw-20990/Amy/GIS_data/Hunter/Maxent_files/ghm.pm/all_variables/output/mean.uncertainty.tif"),cma.mask)
veg.colour <- brewer.pal(9,"Reds")
png("uncertainty.png",height=10,width=10,units="cm",res=300,bg="transparent")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(uncertainty, axes=F, legend=F, box="n", col=veg.colour)
dev.off()


vegetation <- raster(paste0(computer,"/GIS_data/Hunter/Maxent_files/ghm.pm/environmental_data/standardised_rasters/final_vegetation.asc"))
veg.colour <- brewer.pal(9,"Greens")
png("vegetation GH_PM.png",height=10,width=10,units="cm",res=300,bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(vegetation, axes=F, legend=F, box=F, col=veg.colour)
  plot(gh.pm.mask,add=T)
dev.off()

mean.rain <- raster(paste0(computer,"/GIS_data/Hunter/Maxent_files/ghm.pm/environmental_data/standardised_rasters/mean_rain.asc"))
rain.colour <- brewer.pal(9,"Blues")
png("rain.png",height=10,width=10,units="cm",res=300,bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(mean.rain, col=rain.colour,axes=F, legend=F, box=F)
  plot(gh.pm.mask,add=T)
dev.off()

altitude <- raster(paste0(computer,"/GIS_data/Hunter/Maxent_files/ghm.pm/environmental_data/standardised_rasters/altitude.asc"))
topo.colour <- brewer.pal(9,"Greys")
png("altitude.png",height=10,width=10,units="cm",res=300,bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  plot(altitude, col=topo.colour[3:9],axes=F, legend=F, box=F)
  plot(gh.pm.mask,add=T)
dev.off()

Aegotheles_cristatus_GH_PM <- raster(paste0(computer,"/GIS_data/Hunter/zonation/GH_PM/extant/Aegotheles_cristatus_SDM.GH_PM.tif"))
png("GH_PM SDM.png",height=10,width=10,units="cm",res=300,bg="transparent")
  par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  # plot(bg, col=map.background, legend=F, axes=F, box=F)
  plot(Aegotheles_cristatus_GH_PM,col=blue2red(10),legend=F,box=F,axes=F,zlim=c(0,1000))
#   points(sp.points.GH,pch=1,cex=0.5,col=alpha("black",0.5))
  plot(gh.pm.mask,add=T,border="black",lwd=0.5)
#   plot(PM.shp,add=T)
dev.off()

Aegotheles_cristatus_GH <- mask(crop(Aegotheles_cristatus_GH_PM,hccrems.raster),hccrems.raster)

png("GH SDM.png",height=10,width=10,units="cm",res=300,bg="transparent")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(hccrems.raster, col="whitesmoke", legend=F, axes=F, box=F)
plot(Aegotheles_cristatus_GH,col=blue2red(10),legend=F,axes=F,zlim=c(0,1000),add=T)
#   points(sp.points.GH,pch=1,cex=0.5,col=alpha("black",0.5))
plot(GH.shp,add=T,border="black",lwd=0.5)
#   plot(PM.shp,add=T)
dev.off()


GH.priority <- raster("I:/Super Computer Data/GIS_data/Hunter/zonation/greater hunter/output_files/output_5March14_greater.hunter.rank.asc")

top.fraction <- c(0.05, 0.1, 0.15, 0.3)
pri.breaks <- sort(c(0,1-top.fraction,1))
pri.col = c('dark grey', 'turquoise', 'yellow', 'orange', 'red')
pri.labels <- c("top 5%","top 10%","top 15%","top 30%","rest")

png("GH priority.png",height=10,width=10,units="cm",res=300,bg="transparent")
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(hccrems.raster, col="whitesmoke", legend=F, axes=F, box=F)
plot(GH.priority,col=pri.col,breaks=pri.breaks,legend=F,axes=F,zlim=c(0,1000),add=T)
plot(GH.shp,add=T,border="black",lwd=0.5)
dev.off()


ninox <- raster(paste0(computer,"/GIS_data/Hunter/zonation/greater hunter/Ninox_novaeseelandiae.tif"))
ninox.colour <- brewer.pal(9,"Blues")
png("ninox.png",height=10,width=10,units="cm",res=300)
plot(ninox, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
dev.off()

png("ninox_lh.png",height=10,width=10,units="cm",res=300)
plot(ninox, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
plot(lh.mask, add=T)
dev.off()

acacia_bynoeana <- raster(paste0(computer,"/GIS_data/Hunter/zonation/greater hunter/Acacia_bynoeana.tif")
ninox.colour <- brewer.pal(9,"Greens")
png(paste0(computer,"/Teaching/Vegetation Management/Acacia_bynoeana.png",height=10,width=10,units="cm",res=300)
plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
#points(species.data$easting[species.data$species=="Acacia bynoeana"],species.data$northing[species.data$species=="Acacia bynoeana"],pch=".",col="grey")
dev.off()

png(paste0(computer,"/Teaching/Vegetation Management/Acacia_bynoeana_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia bynoeana"],species.data$northing[species.data$species=="Acacia bynoeana"])
dev.off()

acacia_pendula <- raster(paste0(computer,"/GIS_data/Hunter/zonation/greater hunter/Acacia_pendula.tif")
ninox.colour <- brewer.pal(9,"Greens")
png(paste0(computer,"/Teaching/Vegetation Management/Acacia_pendula.png",height=10,width=10,units="cm",res=300)
plot(acacia_pendula, col=ninox.colour,axes=F, legend=T, box="n")
plot(cma.mask,add=T)
#points(species.data$easting[species.data$species=="Acacia pendula"],species.data$northing[species.data$species=="Acacia pendula"],pch=".",col="grey")
dev.off()

png(paste0(computer,"/Teaching/Vegetation Management/Acacia_pendula_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia pendula"],species.data$northing[species.data$species=="Acacia pendula"])
dev.off()

acacia_prominens <- raster(paste0(computer,"/GIS_data/Hunter/zonation/greater hunter/Acacia_prominens.tif")
ninox.colour <- brewer.pal(9,"Greens")
png(paste0(computer,"/Teaching/Vegetation Management/Acacia_prominens.png",height=10,width=10,units="cm",res=300)
plot(acacia_prominens, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,add=T)
# points(species.data$easting[species.data$species=="Acacia prominens"],species.data$northing[species.data$species=="Acacia prominens"],pch=".",col="grey")
dev.off()

png(paste0(computer,"/Teaching/Vegetation Management/Acacia_prominens_points.png",height=10,width=10,units="cm",res=300)
# plot(acacia_bynoeana, col=ninox.colour,axes=F, legend=F, box="n")
plot(cma.mask,axes=F, legend=F, box="n")
points(species.data$easting[species.data$species=="Acacia prominens"],species.data$northing[species.data$species=="Acacia prominens"])
dev.off()



