rm(list=ls())
# species names
names <- as.vector(read.table('//654cw-20990/Amy/GIS_data/Hunter/zonation/lower hunter/lh.zonation.spp', header=F, sep='\t')[,6])
names <- gsub('_LH.tif', '', names)


# define paths for input and output files
input_path <- '//654cw-20990/Amy/GIS_data/Hunter/zonation/lower hunter/Output/'
output_path <- '~/GIS_data/Hunter/zonation/lower hunter/'




# LH General impact assessment

# get names of Zonation curves-files (you can also do this manually)
files <- grep('120514.curves.txt', list.files(input_path), value=T) # remember to update to correct date
files <- files[-9] # removing results for unwanted outputs such as the PA scenario
files <- files[-8]

# create the output table
loss.table <- as.data.frame(matrix(NA, length(files), 12))
dim(loss.table)

# how much area is cleared in each impact scenario?
# you need to finding corresponding values from Zonation outputs (manually) or be smarter and write a code for it :)
cleared_area

cleared_area <- c()


# calculating impact numbers for each scenario
for (h in 1:length(files)){
  
  # Name of original curves file
  input <- paste0(input_path, files[h])
  
  # Upload the curves file
  curves <- read.table(input, skip=1, header=F, sep='')
  colnames(curves) <- c('Prop_landscape_lost', 'cost_needed_for_top_fraction', 'min_prop_rem', 'ave_prop_rem', 'W_prop_rem', 'ext-1', 'ext-2', names)
  a <- curves[curves$Prop_landscape_lost == cleared_area[h], 8:ncol(curves)]
  
  # calculate impacts
  loss.table[h,1] <- sub('.curves.txt', '', files[h])
  loss.table[h,2] <- cleared_area[h]*100
  loss.table[h,3] <- 100-(curves[curves$Prop_landscape_lost == cleared_area[h], 4]*100)
  loss.table[h,4] <- 100-(curves[curves$Prop_landscape_lost == cleared_area[h], 3]*100)
  loss.table[h,5] <- length(which(a == 0))
  loss.table[h,6] <- paste(list(colnames(curves[which(a == 0)+7])), sep=',')
  loss.table[h,7] <- length(which(a > 0 & a <= 0.1))
  loss.table[h,8] <- paste(list(colnames(curves[which(a > 0 & a <= 0.1)+7])), sep=',')
  loss.table[h,9] <- length(which(a > 0.1 & a <= 0.25))
  loss.table[h,10] <- paste(list(colnames(curves[which(a > 0.1 & a <= 0.25)+7])), sep=',')
  loss.table[h,11] <- length(which(a > 0.25 & a <= 0.5))
  loss.table[h,12] <- paste(list(colnames(curves[which(a > 0.25 & a <= 0.5)+7])), sep=',')
}

colnames(loss.table) <- c('Scenario', '%_area_cleared', 'mean_loss(%)', 'max_loss(%)', '100%_loss_#', '100%_loss_sp', '90%_loss_#', '90%_loss_sp', '75%_loss_#', '75%_loss_sp', '50%_loss_#', '50%_loss_sp')
loss.table

write.table(loss.table, paste0(output_path, 'LH_Development plan impacts_160414.txt'), col.names=T, row.names=F, sep='\t')
