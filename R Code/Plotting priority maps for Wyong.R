
library(raster)
library(rgdal)
library(maptools)
library(colorRamps)


# define input path
# super computer
input.path <- ('//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Output/')
# laptop
input.path <- ('C:/Users/hkujala/work/RSPs/Wyong/Zonation')



# load PA mask and calculate proportion of NPs and State Forests
PA <- raster('//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Mask files/PA_mask.tif')
np <- freq(PA)[3,2]/sum(freq(PA)[1:3,2])
sf <- freq(PA)[2,2]/sum(freq(PA)[1:3,2])

# load curves files of the solution and find the fractions that mark protected areas
curves.PA <- read.table(paste0(input.path, 'output_wyong_PAs_w5.curves.txt'), sep='')
protected.fraction <- curves.PA$V1[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf)))))]
np.fraction <- curves.PA$V1[which(abs(curves.PA$V1-(1-(np)))==min(abs(curves.PA$V1-(1-(np)))))]

# define top fractions of interest
top.fraction <- protected.fraction - c(0.05, 0.1, 0.15, 0.3)



# Create maps of priority sites
# NB that this code splits the rank layer to two (unprotected areas & protected areas)

# background map
bg <- raster('//654cw-20990/Amy/GIS_data/Hunter/mask files/wyong.lakes.mask.tif')

# rank map
pri.pa <- raster(paste0(input.path, 'output_wyong_PAs_w5.rank.compressed.tif'))

# breaks used to categorize cells
pri.breaks <- c(0, top.fraction[order(top.fraction)], protected.fraction)
pa.breaks = c(protected.fraction, np.fraction, 1)

# unprotected cells
pri <- pri.pa
pri[which(pri[] > protected.fraction)] <- NA

# protected cells
pa <- pri.pa
pa[which(pa[] < protected.fraction)] <- NA


# plotting
pri.col = c('dark grey', 'turquoise', 'yellow', 'orange', 'red')
pa.col = c('palegreen3', 'palegreen4')

leg.labels <- c("top 5%","top 10%","top 15%","top 30%","rest")

png('PA.expansion.priorities.w5.png', width = 11, height = 10, units="cm",res=300,bg="transparent",pointsize=10)

par(mfrow=c(1,1), mar=c(4,1,1,0), oma=c(0,0,0,0))
plot(bg, col='light grey', legend=F, axes=F, box=F)
plot(pri, breaks = pri.breaks, col=pri.col, add=T, legend=F, box=F)
plot(pa, breaks = pa.breaks, col=pa.col, add=T, legend=F, box=F)
legend('bottom', inset=c(0,-0.18), leg.labels, col=rev(pri.col), pch=15, bty="n", title="Conservation priority", cex=1, xpd=NA, horiz=T, title.adj=0)

dev.off()
