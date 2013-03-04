Extract records from Australia Living Atlas data
========================================================
This script extracts records of threatened species (& other species we are interested in) from the data files downloaded from the Australian Living Atlas for the Hunter Valley region. 

Data were downloaded by selecting the Hunter region and then downloading all records within the region in large taxonomic blocks.  
 
The script uses a function `extract` to identify whether data for each species in `species.list` has been downloaded from the ALA database based on the scientific name.  Note that sub-species will be lumped together unless they are specifically listed in `species.list`. If data exists, the records for each species are saved as a separate csv file by their scientific name.  The master threatened species file is updated to reflect that the data has been downloaded and lists the number of records for that species.  

The script loops through each taxonomic group and extracts the records for species listed in `threatened species`.  A print out to the screen lists the number of records successfully downloaded for each available species.


```r
# set working directory
rm(list = ls())
setwd("C:/Users/awhitehead/Documents/GIS data/Hunter/species point data")

# open the list of threatened and interesting species for potential
# analysis
threatened.species <- read.csv("Nationally listed threatened species.csv")
species.list <- threatened.species$Scientific.Name[threatened.species$ALA.records > 
    0]

# the taxanomic blocks that the data were downloaded in
taxa <- c("mammals", "amphibians", "plants", "birds", "reptiles")

# function extract species data
extract <- function(species) {
    records <- ALA.records[grep(species, ALA.records$Scientific.Name), ]
    if (nrow(records) > 0) {
        threatened.species$Downloaded.ALA[threatened.species$Scientific.Name == 
            species] <<- "yes"
        threatened.species$ALA.records[threatened.species$Scientific.Name == 
            species] <<- nrow(records)
        write.csv(records, glue("ALA data/", species, ".csv"), row.names = FALSE)
        cat(nrow(records), "records for", as.character(species), "successfully extracted", 
            "\n")
    }
}

# loop through each taxonomic group and extract records based on the
# threatened species list
for (k in seq(taxa)) {
    ALA.records <- read.csv(glue("ALA downloads/", taxa[k], ".csv"))
    cat("\n", taxa[k], "\n")
    for (i in seq(species.list)) {
        extract(species.list[i])
    }
}
```

```
## 
##  mammals 
## 7941 records for Phascolarctos cinereus successfully extracted 
## 3337 records for Pteropus poliocephalus successfully extracted 
## 1902 records for Dasyurus maculatus successfully extracted 
## 1230 records for Petaurus australis successfully extracted 
## 926 records for Pseudomys novaehollandiae successfully extracted 
## 919 records for Petaurus norfolcensis successfully extracted 
## 294 records for Petrogale penicillata successfully extracted 
## 133 records for Chalinolobus dwyeri successfully extracted 
## 77 records for Potorous tridactylus successfully extracted 
## 39 records for Pseudomys oralis successfully extracted 
## 
##  amphibians 
## 936 records for Litoria aurea successfully extracted 
## 520 records for Mixophyes balbus successfully extracted 
## 137 records for Mixophyes iteratus successfully extracted 
## 117 records for Heleioporus australiacus successfully extracted 
## 90 records for Litoria booroolongensis successfully extracted 
## 44 records for Litoria littlejohni successfully extracted 
## 
##  plants 
## 1158 records for Rutidosis heterogama successfully extracted 
## 1081 records for Eucalyptus parramattensis subsp. decadens successfully extracted 
## 910 records for Angophora inopina successfully extracted 
## 633 records for Grevillea parviflora subsp. parviflora successfully extracted 
## 585 records for Melaleuca biconvexa successfully extracted 
## 421 records for Eucalyptus glaucina successfully extracted 
## 282 records for Acacia bynoeana successfully extracted 
## 219 records for Tasmannia purpurascens successfully extracted 
## 205 records for Prostanthera junonis successfully extracted 
## 150 records for Syzygium paniculatum successfully extracted 
## 116 records for Prostanthera askania successfully extracted 
## 90 records for Allocasuarina simulans successfully extracted 
## 73 records for Diuris venosa successfully extracted 
## 69 records for Cynanchum elegans successfully extracted 
## 65 records for Persoonia pauciflora successfully extracted 
## 64 records for Eucalyptus pumila successfully extracted 
## 62 records for Diuris praecox successfully extracted 
## 62 records for Prostanthera densa successfully extracted 
## 59 records for Asperula asthenes successfully extracted 
## 57 records for Allocasuarina defungens successfully extracted 
## 50 records for Homoranthus darwinioides successfully extracted 
## 47 records for Cryptostylis hunteriana successfully extracted 
## 44 records for Eucalyptus camfieldii successfully extracted 
## 41 records for Tasmannia glaucifolia successfully extracted 
## 38 records for Kennedia retrorsa successfully extracted 
## 24 records for Grevillea guthrieana successfully extracted 
## 19 records for Ozothamnus tesselatus successfully extracted 
## 18 records for Hakea archaeoides successfully extracted 
## 17 records for Prostanthera discolor successfully extracted 
## 15 records for Pomaderris reperta successfully extracted 
## 11 records for Diuris pedunculata successfully extracted 
## 11 records for Prostanthera cineolifera successfully extracted 
## 10 records for Pterostylis gibbosa successfully extracted 
## 10 records for Philotheca ericifolia successfully extracted 
## 9 records for Prostanthera stricta successfully extracted 
## 7 records for Prostanthera cryptandroides subsp. cryptandroides successfully extracted 
## 7 records for Rhizanthella slateri successfully extracted 
## 7 records for Caladenia tessellata successfully extracted 
## 7 records for Olearia cordata successfully extracted 
## 6 records for Tylophora woollsii successfully extracted 
## 5 records for Diuris bracteata successfully extracted 
## 5 records for Eucalyptus cannonii successfully extracted 
## 5 records for Persicaria elatior successfully extracted 
## 4 records for Astrotricha crassifolia successfully extracted 
## 4 records for Pomaderris brunnea successfully extracted 
## 4 records for Prasophyllum fuscum successfully extracted 
## 4 records for Pterostylis cucullata successfully extracted 
## 4 records for Thesium australe successfully extracted 
## 3 records for Euphrasia arguta successfully extracted 
## 3 records for Baeckea kandos successfully extracted 
## 3 records for Eucalyptus nicholii successfully extracted 
## 3 records for Haloragis exalata subsp. velutina successfully extracted 
## 2 records for Genoplesium littorale successfully extracted 
## 2 records for Darwinia biflora successfully extracted 
## 2 records for Grevillea shiressii successfully extracted 
## 2 records for Pomaderris sericea successfully extracted 
## 1 records for Prostanthera marifolia successfully extracted 
## 1 records for Grevillea obtusiflora successfully extracted 
## 1 records for Parsonsia dorrigoensis successfully extracted 
## 1 records for Persoonia hirsuta successfully extracted 
## 1 records for Zieria lasiocaulis successfully extracted 
## 1 records for Acacia courtii successfully extracted 
## 1 records for Acacia pubescens successfully extracted 
## 1 records for Macadamia integrifolia successfully extracted 
## 1 records for Marsdenia longiloba successfully extracted 
## 1 records for Velleia perfoliata successfully extracted 
## 
##  birds 
## 536 records for Ninox strenua successfully extracted 
## 292 records for Tyto novaehollandiae successfully extracted 
## 253 records for Lathamus discolor successfully extracted 
## 221 records for Tyto tenebricosa successfully extracted 
## 39 records for Botaurus poiciloptilus successfully extracted 
## 34 records for Anthochaera phrygia successfully extracted 
## 12 records for Rostratula australis successfully extracted 
## 5 records for Erythrotriorchis radiatus successfully extracted 
## 
##  reptiles 
## 2 records for Hoplocephalus bungaroides successfully extracted 
## 1 records for Aprasia parapulchella successfully extracted
```

```r

# update threatened species download information
write.csv(threatened.species, "Nationally listed threatened species.csv", row.names = FALSE)
```


Note that this markdown file doesn't run properly unless all the R code is in one chunk.  Not sure what is going on there.  
*This file was last updated on 25 January 2013 and last run on 25 January 2013.*
