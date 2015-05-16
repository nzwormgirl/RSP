
rm(list=ls())
ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}
## don't forget to check the computer setting within each Rmd file and that the paths point to the appropriate models
computer <- "Z:/Amy/"
#computer <- "//654cw-20990/Amy"

## Run the inital models at the GH-PM scale

# extract all records from the original ALA data
ksource(paste0(computer,"/R code/extract all records by taxa.Rmd" ))

# create the maxent data files
ksource(paste0(computer,"/R code/create maxent and zonation datafiles.Rmd" ))

# Run MaxEnt models from batch files

# Run TEC BRT models
ksource(paste0(computer,"/R code/run TEC BRT models.Rmd" ))

# Convert SSI files to tifs at Greater Hunter scale
ksource(paste0(computer,"/R code/convert ssi txt files to tif rasters.Rmd" ))

# 1. Clip the MaxEnt output to the Greater Hunter, saving both the extant & pre1750 layers
ksource(paste0(computer,"/R code/convert asc files to tif for zonation.Rmd" ))


## Cut models to the Lower Hunter

#2. Create the SEWPAC special layers
ksource(paste0(computer,"R code/convert SEWPAC species files into LH zonation format.Rmd"))

#2. Clip the extant & pre1750 data to the Lower Hunter & generating SSI files for species & TECs
ksource(paste0(computer,"/R code/clip files to LHSA boundaries.rmd"))


## Cut models to the Upper Hunter

# Clip to Upper hunter
ksource(paste0(computer,"/R code/clip files to UHSA boundaries.rmd"))


## Cut models to Wyong

# Clip to Wyong
ksource(paste0(computer,"/R code/clip files to wyong boundaries.rmd"))

# Create Wyong summary tables
ksource(paste0(computer,"/R code/Maxent summaries for wyong report.rmd"))


## Cut models to Lower Hunter Coal

# Clip to Lower Hunter Coal
ksource(paste0(computer,"/R code/clip files to Lower Hunter coal.rmd"))

# Create Lower Hunter Coal summary tables
ksource(paste0(computer,"/R code/Maxent summaries for Lower Hunter Coal paper.rmd"))

