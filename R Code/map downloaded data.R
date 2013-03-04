## tidy up ALA records
rm(list=ls())
# packages(raster)
# packages(RSAGA)
# packages(RColorBrewer)
# packages(maptools)
# packages(fields)
packages(ggplot2)

setwd("C:/Users/awhitehead/Documents/GIS data/Hunter/species point data/")

species <- c("Pteropus poliocephalus","Rutidosis heterogama","Eucalyptus parramattensis subsp. decadens","Litoria aurea","Pseudomys novaehollandiae","Angophora inopina", "Grevillea parviflora subsp. parviflora","Lathamus discolor", "Melaleuca biconvexa","Mixophyes balbus")

# open species list
threatened.species <- read.csv("Nationally listed threatened species.csv")
species.list <- threatened.species$Scientific.Name[threatened.species$ALA.records>100]

combined.data <- d(species=NULL,longitude=NULL,latitude=NULL)
zonation.spp <- d(weight=rep(1.0,length(species)), alpha=1.0, bqp.row=1.0, bqp.buffer=1.0, cell.removal=1.0, species=NA )
for (i in seq(species)){
  ALA.data <- read.csv(glue("ALA data/",species[i],".csv"))
  colnames(ALA.data)[c(20:21)] <- c("latitude", "longitude")
  ALA.data$index <- paste(sprintf("%.6f",ALA.data$latitude), sprintf("%.6f",ALA.data$longitude))
  ALA.data$database <- "ALA"
  ALA.data$species <- species[i]

NSW.data <- read.table(glue("NSW Atlas data/",species[i],".txt"), sep="\t",header=TRUE, skip=4)
  colnames(NSW.data)[c(21:22)] <- c("latitude", "longitude")
  NSW.data$index <- paste(sprintf("%.6f",NSW.data$latitude), sprintf("%.6f",NSW.data$longitude))
  NSW.data$database <- "NSW"
  NSW.data$species <- species[i]

combined.data <- rbind(combined.data,ALA.data[,c("species","longitude","latitude")],NSW.data[,c("species","longitude","latitude")])
  
 zonation.spp$species[i] <- glue(species[i],".asc")
  
}

unique.data <- combined.data[which(!duplicated(combined.data$index)==TRUE),]

map <- get_map(location = c(151.573,-32.490), zoom = 8, maptype="hybrid", color="bw")


(species.map <- ggmap(map) + geom_point(aes(x = longitude, y = latitude, colour=species), data = combined.data, alpha = .5))
                                        


(species.map <- ggplot(combined.data, aes(longitude, latitude, group=species)) + geom_point(shape=1,alpha=0.5, aes(colour=species)) )

write.csv(combined.data, glue("maxent data/combined.data.csv"),row.names=FALSE)
write.table(zonation.spp,glue("maxent data/zonation.spp.txt"),sep="\t",row.names=FALSE, col.names=FALSE)

# create generic spp file for zonation
