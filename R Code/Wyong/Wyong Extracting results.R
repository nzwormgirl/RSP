
library(raster)
library(rgdal)
library(maptools)


# define input path
# super computer
input.path <- ('//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Output/')
# laptop
input.path <- ('C:/Users/hkujala/work/RSPs/Wyong/Zonation')

# load list of species and their distribution sizes
features <- gsub('_SSI_wyong.tif|_SDM_wyong.tif|.tif$', '', as.vector(read.table(paste0(input.path, 'output_wyong.features_info.txt'), skip=2, sep='\t')[,7]))
dist.sums <- as.vector(as.vector(read.table(paste0(input.path, 'output_wyong.features_info.txt'), skip=2, sep='\t')[,2]))


### Gap analysis

PA <- raster('//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Mask files/PA_mask.tif')

# proportion of NPs and State Forests
np <- freq(PA)[3,2]/sum(freq(PA)[1:3,2])
sf <- freq(PA)[2,2]/sum(freq(PA)[1:3,2])

curves.PA <- read.table(paste0(input.path, 'output_wyong_PAs_w5.curves.txt'), sep='')

# mean % of high protection (in NPs)
curves.PA[which(abs(curves.PA$V1-(1-np))==min(abs(curves.PA$V1-(1-np)))),4]
# min % of high protection (in NPs)
curves.PA[which(abs(curves.PA$V1-(1-np))==min(abs(curves.PA$V1-(1-np)))),3]
# which species are missing entirely from NPs?
np.gap.sp <- which(curves.PA[which(abs(curves.PA$V1-(1-np))==min(abs(curves.PA$V1-(1-np)))),8:ncol(curves.PA)]==0)
np.gap.sp.names <- gsub('_', ' ', features[np.gap.sp])


# mean % with any protection (either in NPs or SFs)
curves.PA[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf))))),4]
# min % of high protection (in NPs)
curves.PA[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf))))),3]
# which species are missing entirely?
gap.sp <- which(curves.PA[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf))))),8:ncol(curves.PA)]==0)
gap.sp.names <- gsub('_', ' ', features[gap.sp])

# GAP species
gap.sp.table <- merge(table.full, as.data.frame(np.gap.sp.names), by.x='Scientific.Name', by.y='np.gap.sp.names')
gap.sp.table$NP.GAP <- TRUE
gap.sp.table$GAP <- gap.sp.table$Scientific.Name %in% gap.sp.names

write.table(gap.sp.table, 'Gap_species_final.txt', col.names=T, row.names=F, sep='\t')

# STILL TO BE DONE
for (j in seq(top.fraction)){
  table[,j+col] <- as.numeric(curves[which(abs(curves$V1-top.fraction[j])==min(abs(curves$V1-top.fraction[j]))),8:ncol(curves)])
}


# TOP PRIORITIES 

# NO CONSIDERATION OF PAs, EXPLORING DIFFERENT WEIGHTINGS

## Creating species-specific tables

# % of distribution captured by top priorities
curves <- read.table(paste0(input.path, 'output_wyong.curves.txt'), sep='')
weights <- c('','.w2','.w5','.w10')

table <- matrix(NA, length(features), 2+(4*length(weights))) # NB! there are couple of sp that got removed from final runs - control for the number of features and rows in this code
table[,2] <- dist.sums

top.fraction <- 1-c(0.05,0.1, 0.15, 0.3)
col <- 2
for (i in seq(weights)){
  curves <- read.table(paste0(input.path, 'output_wyong', weights[i], '.curves.txt'), sep='')
  for (j in seq(top.fraction)){
    table[,j+col] <- as.numeric(curves[which(abs(curves$V1-top.fraction[j])==min(abs(curves$V1-top.fraction[j]))),8:ncol(curves)])
  }
  col <- col + 4
}

length(as.numeric(curves[which(abs(curves$V1-top.fraction[j])==min(abs(curves$V1-top.fraction[j]))),8:ncol(curves)]))

colnames(table) <- c('Feature', 'DistributionSize', 'Top5.w1', 'Top10.w1', 'Top15.w1', 'Top30.w1', 'Top5.w2', 'Top10.w2', 'Top15.w2', 'Top30.w2', 'Top5.w5', 'Top10.w5', 'Top15.w5', 'Top30.w5', 'Top5.w10', 'Top10.w10', 'Top15.w10', 'Top30.w10')
table <- as.data.frame(table)
table[,1] <- features

# Species area-representation at top fraction-plot
plot(table[,2], table[,5], ylim=c(0,1), xlab='Distribution size', ylab='Proportion of distribution covered', pch=19, col='turquoise', bty='n')
points(table[,2], table[,4], pch=19, col='orange')
points(table[,2], table[,3], pch=19, col='red')
legend('topright', legend=c('top 10%', 'top 15%', 'top 30%') , pch=19, col=c('red', 'orange', 'turquoise'), bty='n', horiz=T)


# write out results
write.table(table, 'Sp_proportions_at_top_fractions.txt', col.names=T, row.names=F, sep='\t')



# PRIORITIES BEYOND CURRENT PAs, WITH FINAL WEIGHTING (X5)
curves.PA <- read.table(paste0(input.path, 'output_wyong_PAs_w5.curves.txt'), sep='')

table <- matrix(NA, length(features), 13)
colnames(table) <- c('Scientific.Name', 'DistributionSize', 'Protection.NP', 'Protection.SF', 'Protection.All', 'Top5.w5', 'Top10.w5', 'Top15.w5', 'Top30.w5', 'FinalProt.top5', 'FinalProt.top10', 'FinalProt.top15', 'FinalProt.top30')
table[,2] <- dist.sums
table[,3] <- as.numeric(curves.PA[which(abs(curves.PA$V1-(1-np))==min(abs(curves.PA$V1-(1-np)))),8:ncol(curves.PA)])
table[,5] <- as.numeric(curves.PA[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf))))),8:ncol(curves.PA)])
table[,4] <- table[,5]-table[,3] 
  
protected.fraction <- curves.PA$V1[which(abs(curves.PA$V1-(1-(np+sf)))==min(abs(curves.PA$V1-(1-(np+sf)))))]
np.fraction <- curves.PA$V1[which(abs(curves.PA$V1-(1-(np)))==min(abs(curves.PA$V1-(1-(np)))))]
top.fraction <- protected.fraction - c(0.05, 0.1, 0.15, 0.3)

for (j in seq(top.fraction)){
    table[,9+j] <- as.numeric(curves.PA[which(abs(curves.PA$V1-top.fraction[j])==min(abs(curves.PA$V1-top.fraction[j]))),8:ncol(curves.PA)])
    table[,5+j] <- table[,9+j]-table[,5]
}

table <- as.data.frame(table)
table[,1] <- features



# merge with other species information
table[,1] <- gsub('_', ' ', table[,1])
sp.info.mega <- read.table('C:/Users/hkujala/work/RSPs/IBRA threatened species list.csv', header=T, sep=',')

columns <- c('Taxa', 'Family', 'Scientific.Name', 'Common.Name', 'mnes', 'NSW.status', 'Comm.status')
sp.info.short <- sp.info.mega[,which(colnames(sp.info.mega) %in% columns)]

table.full <- merge(sp.info.short, table, by.x='Scientific.Name', by.y='Feature', all.y=T)


# write out results
write.table(table.full, 'Sp_proportions_at_top_fractions.txt', col.names=T, row.names=F, sep='\t')


# Create maps of priority sites
library(colorRamps)
top.fraction <- 1-c(0.05, 0.1, 0.15, 0.3)
bg <- raster('//654cw-20990/Amy/GIS_data/Hunter/mask files/wyong.lakes.mask.tif')
bg <- trim(bg)

# Unconstrained priorities
# how much of top 5,10,15,30% sites currently protected or unprotected

pri <- raster(paste0(input.path, 'output_wyong_final.rank.compressed.tif'))
pri <- trim(pri)
pri.w5 <- raster(paste0(input.path, 'output_wyong.w5_final.rank.compressed.tif'))
pri.w5 <- trim(pri.w5)
pri.breaks <- c(0, top.fraction[order(top.fraction)], 1)

# How large % of Wyong is included to the prioritization?
ncell(pri[!is.na(pri)])/ncell(bg[!is.na(bg)])

# map of remnant native veg

par(mar=c(0,0,0,0))
plot(bg, col='grey75', axes=F, bty='n', box=F, legend=F)
plot(pri, col='#4C9900', add=T, legend=F, alpha=0.7)


pri.col = c('dark grey', 'turquoise', 'yellow', 'orange', 'red')
leg.labels <- c("top 5%","top 10%","top 15%","top 30%","rest")


tiff(file='Conservation priorities_Wyong_final.tif', width = 15, height = 8, units="cm",res=300, compression='lzw', bg="transparent",pointsize=12)
#png(file='Conservation priorities_Wyong_final.png', width = 15, height = 8, units="cm",res=300,bg="transparent",pointsize=12)
par(mfrow=c(1,2), mar=c(0,0,0,0), oma=c(3,1,0,0), cex=0.9)
plot(bg, col='light grey', legend=F, axes=F, box=F, xlim=c(332100,372300), ylim=c(6299000,6343000))
plot(pri, breaks = pri.breaks, col=pri.col, add=T, legend=F, box=F)
text(332100,6341000, labels='A', xpd=NA)

plot(bg, col='light grey', legend=F, axes=F, box=F, xlim=c(332100,372300), ylim=c(6299000,6343000))
plot(pri.w5, breaks = pri.breaks, col=pri.col, add=T, legend=F, box=F)
text(332100,6341000, labels='B', xpd=NA)

legend(285000,6302000, leg.labels, col=rev(pri.col), pch=15, bty="n", title="Biodiversity priority", cex=0.9, xpd=NA, horiz=T, title.adj=0)

dev.off()


# how much of the top priorities is currently protected?

pa.mask <- raster('//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Mask files/PA_mask.tif')
pri.protection <- matrix(NA, length(top.fraction), 6)
colnames(pri.protection) <- c('Top priorities', 'Area (ha)', 'Protected by NPs', 'Protected by SFs', 'Total protection', 'Not protected')
prop <- (1-top.fraction)*100
pri.protection[,1] <- prop

# weighted solution
pri.w5.5 <- pri.w5 > top.fraction[1]
pri.w5.10 <- pri.w5 > top.fraction[2]
pri.w5.15 <- pri.w5 > top.fraction[3]
pri.w5.30 <- pri.w5 > top.fraction[4]

pri.w5.5.np <- pri.w5.5 * (pa.mask == 7)
pri.w5.10.np <- pri.w5.10 * (pa.mask == 7)
pri.w5.15.np <- pri.w5.15 * (pa.mask == 7)
pri.w5.30.np <- pri.w5.30 * (pa.mask == 7)

pri.w5.5.sf <- pri.w5.5 * (pa.mask == 6)
pri.w5.10.sf <- pri.w5.10 * (pa.mask == 6)
pri.w5.15.sf <- pri.w5.15 * (pa.mask == 6)
pri.w5.30.sf <- pri.w5.30 * (pa.mask == 6)


for (i in seq(top.fraction)){
  pri.protection[i,2] <- sum(get(sprintf("pri.w5.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,3] <- sum(get(sprintf("pri.w5.%s.np", prop[i]))@data@values, na.rm=T)/sum(get(sprintf("pri.w5.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,4] <- sum(get(sprintf("pri.w5.%s.sf", prop[i]))@data@values, na.rm=T)/sum(get(sprintf("pri.w5.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,5] <- sum(pri.protection[i,3:4])
  pri.protection[i,6] <- 1-pri.protection[i,5]
}

write.table(pri.protection, 'TopPriorities_protected_w5.txt', col.names=T, row.names=F, sep='\t')


# unweighted solution
pri.5 <- pri > top.fraction[1]
pri.10 <- pri > top.fraction[2]
pri.15 <- pri > top.fraction[3]
pri.30 <- pri > top.fraction[4]

pri.5.np <- pri.5 * (pa.mask == 7)
pri.10.np <- pri.10 * (pa.mask == 7)
pri.15.np <- pri.15 * (pa.mask == 7)
pri.30.np <- pri.30 * (pa.mask == 7)

pri.5.sf <- pri.5 * (pa.mask == 6)
pri.10.sf <- pri.10 * (pa.mask == 6)
pri.15.sf <- pri.15 * (pa.mask == 6)
pri.30.sf <- pri.30 * (pa.mask == 6)


for (i in seq(top.fraction)){
  pri.protection[i,2] <- sum(get(sprintf("pri.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,3] <- sum(get(sprintf("pri.%s.np", prop[i]))@data@values, na.rm=T)/sum(get(sprintf("pri.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,4] <- sum(get(sprintf("pri.%s.sf", prop[i]))@data@values, na.rm=T)/sum(get(sprintf("pri.%s", prop[i]))@data@values, na.rm=T)
  pri.protection[i,5] <- sum(pri.protection[i,3:4])
  pri.protection[i,6] <- 1-pri.protection[i,5]
}

write.table(pri.protection, 'TopPriorities_protected_unweighted.txt', col.names=T, row.names=F, sep='\t')




# Expansion of PAs
pri.pa <- raster(paste0(input.path, 'output_wyong_PAs_w5.rank.compressed.tif'))
pri.breaks <- c(0, top.fraction[order(top.fraction)], protected.fraction)
pri.breaks <- c(top.fraction[order(top.fraction)], protected.fraction) # when not including 'rest'
pa.breaks = c(protected.fraction, np.fraction, 1)


pri <- pri.pa
plot(pri)
pri[which(pri[] > protected.fraction)] <- NA

pa <- pri.pa
pa[which(pa[] < protected.fraction)] <- NA

pri.col = c('dark grey', 'turquoise', 'yellow', 'orange', 'red')
pa.col = c('palegreen3', 'palegreen4')
pri.col = c('turquoise', 'yellow', 'orange', 'red')


leg.labels <- c("very high","high","moderately high","moderate")
leg.labels <- c("top 5%","top 10%","top 15%","top 30%","rest")

# Basic figure for the analysis flow chart
png('Expansion_priorities_Wyong.png', width = 12, height = 12, units="cm",res=300,bg="transparent",pointsize=12)
par(mfrow=c(1,1), mar=c(1.2,1.2,1.2,1.2), oma=c(3,3,0,0))
#par(par.default)
plot(bg, col='light grey', legend=F, axes=F, box=F)
plot(pri, breaks = pri.breaks, col=pri.col, add=T, legend=F, box=F)
plot(pa, breaks = pa.breaks, col=pa.col, add=T, legend=F, box=F)
legend('bottom', inset=c(-0.1,-0.12), leg.labels, col=rev(pri.col), pch=15, bty="n", title="Biodiversity priority", cex=1.1, horiz=T, title.adj=0, xpd=NA)

dev.off()

# Big PDF map (control the size)
png('Conservation_priorities_Wyong.png', width = 12, height = 12, units="cm",res=300,bg="transparent",pointsize=12)
par(mfrow=c(1,1), mar=c(2,1,3,0), oma=c(0,0,0,0))
plot(bg, col='light grey', legend=F, axes=F, box=F, main=paste('Environmental value based on', '\n', 'all flora and fauna species'))
plot(pri, breaks = pri.breaks, col=pri.col, add=T, legend=F, box=F)
#plot(pa, breaks = pa.breaks, col=pa.col, add=T, legend=F, box=F)
#legend('bottom', inset=c(0,-0.18), leg.labels, col=rev(pri.col), pch=15, bty="n", title="Conservation priority", cex=1, xpd=NA, horiz=T, title.adj=0)

legend('bottomright', inset=c(-0.18,0), leg.labels, col=rev(pri.col), pch=15, bty="n", cex=1, xpd=NA)
mtext('This map is an output of modelling study undertaken by the University of Melbourne (2014), 
commissioned as part of the Strategic Development Lands Biodiversity Certification Assessment. 
High environmental value areas are priorities for site surveys to determine offset credit value.', side=1, cex=0.5, outer=T, line=-1.25)
dev.off()

col <- grep('green', colors(), value=T)
pie(rep(1,length(col)), col=col, labels=col)

help(mtext)

# save a raster of priorities classified to the top fractions

pri[which(pri[] < pri.breaks[2])] <- 5
pri[which(pri[] < pri.breaks[3])] <- 4
pri[which(pri[] < pri.breaks[4])] <- 3
pri[which(pri[] < pri.breaks[5])] <- 2
pri[which(pri[] < pri.breaks[6])] <- 1

plot(pri, col=rev(pri.col))

writeRaster(pri, 'PA.expansion.priorities.w5.tif', format='GTiff', overwrite=T)


# merge with other species information
table[,1] <- gsub('_', ' ', table[,1])
sp.info <- read.table('Wyong species.txt', header=T, sep='\t')

  # mergin with mega table
sp.info.mega <- read.table('C:/Users/hkujala/work/RSPs/IBRA threatened species list.csv', header=T, sep=',')

colnames(sp.info.mega)
columns <- c('Taxa', 'Family', 'Scientific.Name', 'Common.Name', 'mnes', 'NSW.status', 'Comm.status')
sp.info.short <- sp.info.mega[,which(colnames(sp.info.mega) %in% columns)]

table.full <- merge(table, sp.info.short, by.x='Feature', by.y='Scientific.Name', all.x=T)
write.table(table.full, 'Sp_proportions_at_top_fractions.txt', col.names=T, row.names=F, sep='\t')

  # mergin with previous results
sp.info <- read.table('C:/Users/hkujala/work/RSPs/Wyong/Sp_proportions_at_top_fractions_AllResults.txt', header=T, sep='\t')
setdiff(table[,1], sp.info[,3])

table.full <- merge(sp.info, table[,-2], by.y='Feature', by.x='Scientific.Name', all.x=T)
write.table(table.full, 'C:/Users/hkujala/work/RSPs/Wyong/Sp_proportions_at_top_fractions_AllResults.txt', col.names=T, row.names=F, sep='\t')
table.full <- read.table('C:/Users/hkujala/work/RSPs/Wyong/Sp_proportions_at_top_fractions_AllResults.txt', header=T, sep='\t')

# boxplots of weighting impacts
require(reshape2)
require(ggplot2)

colnames(table.full)
data <- table.full[,c(14,16:27)]
data.m <- melt(data, id.var = "weighted")
ggplot(data = data.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=weighted))
help(ggplot)



### Development impacts

mask.path <- '//654cw-20990/Amy/GIS_data/Hunter/zonation/wyong/Mask files/'

# % of landscape developed
dev.plan <- raster(paste0(mask.path, 'Dev_planned.tif'))
dev.lep <- raster(paste0(mask.path, 'Dev_LEPs_maj.tif'))
dev.all <- raster(paste0(mask.path, 'Dev_all.tif'))

cleared_area <- as.numeric(freq(dev.plan)[1,2]/sum(freq(dev.plan)[1:2,2]))
cleared_area <- c(cleared_area, as.numeric(freq(dev.lep)[1,2]/sum(freq(dev.lep)[1:2,2])))
cleared_area <- c(cleared_area, sum(freq(dev.all)[1:2,2])/sum(freq(dev.all)[1:3,2]))


# Name of original curves file
files <- c('output_wyong_DevPlanned_w5_final.curves.txt', 'output_wyong_DevLEPs_w5_final.curves.txt', 'output_wyong_DevAll_w5_final.curves.txt')
input <- paste0(input.path, files)

# create the output table
loss.table <- as.data.frame(matrix(NA, length(input), 12))
dim(loss.table)


# calculating impact numbers for each scenario
for (h in seq(input)){

  # Upload the curves file
  curves <- read.table(input[h], sep='')
#  colnames(curves) <- c('Prop_landscape_lost', 'cost_needed_for_top_fraction', 'min_prop_rem', 'ave_prop_rem', 'W_prop_rem', 'ext-1', 'ext-2', names)
  a <- curves[which(curves$V1>cleared_area[h])[1], 8:ncol(curves)]
  
  # calculate impacts
  loss.table[h,1] <- gsub('output_wyong_', '', gsub('.curves.txt', '', files[h]))
  loss.table[h,2] <- cleared_area[h]*100
  loss.table[h,3] <- 100-(curves[which(curves$V1>cleared_area[h])[1], 4]*100)
  loss.table[h,4] <- 100-(curves[which(curves$V1>cleared_area[h])[1], 3]*100)
  loss.table[h,5] <- length(which(a == 0))
  loss.table[h,6] <- paste(list(features[which(a == 0)]), sep=',')
  loss.table[h,7] <- length(which(a > 0 & a <= 0.1))
  loss.table[h,8] <- paste(list(features[which(a > 0 & a <= 0.1)]), sep=',')
  loss.table[h,9] <- length(which(a > 0.1 & a <= 0.25))
  loss.table[h,10] <- paste(list(features[which(a > 0.1 & a <= 0.25)]), sep=',')
  loss.table[h,11] <- length(which(a > 0.25 & a <= 0.5))
  loss.table[h,12] <- paste(list(features[which(a > 0.25 & a <= 0.5)]), sep=',')
}


colnames(loss.table) <- c('Scenario', '%_area_cleared', 'mean_loss(%)', 'max_loss(%)', '100%_loss_#', '100%_loss_sp', '90%_loss_#', '90%_loss_sp', '75%_loss_#', '75%_loss_sp', '50%_loss_#', '50%_loss_sp')


write.table(loss.table, 'Wyong_Development plan impacts_w5_final.txt', col.names=T, row.names=F, sep='\t')

# species-specific results (together with protection)
table <- matrix(NA, length(features), 6)
for (h in seq(input)){
  curves <- read.table(input[h], sep='')
  table[,h+3] <- 1-as.numeric(curves[which(curves$V1>cleared_area[h])[1], 8:ncol(curves)])
}



load('C:/Users/hkujala/work/Mining offsets/Breaks')
offset.breaks
