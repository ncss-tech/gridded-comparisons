library(aqp)
library(soilDB)
library(jsonlite)
library(reshape2)
library(digest)
library(lattice)
library(tactile)
library(cluster)
library(sharpshootR)

library(spData)
library(sp)
library(sf)
library(rgdal)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.98821,-120.39788,z15
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.37110,-82.02573,z15
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.37093,-82.03019,z15
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=,,z15

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=,,z15

source('local-functions.R')


data("us_states")
us <- as(us_states, 'Spatial')


s <- spsample(us, n=1, type='random')
cd <- coordinates(s)

g <- prepData(x = cd[, 1], y = cd[, 2], gl = sprintf("(%0.4f, %0.4f)", cd[, 1], cd[, 2]))

# this will get you there
url <- sprintf("https://casoilresource.lawr.ucdavis.edu/gmap/?loc=%s,%s", cd[, 2], cd[, 1])
browseURL(url)


##
## Arches
##


# g <- prepData(x = -109.67960, y = 38.81925, gl = 'Arches')

##
## Clovis, CA
##

g <- prepData(x = -119.72330, y = 36.92204, gl = 'Clovis, CA')


par(mar=c(0,0,3,0))

groupedProfilePlot(g, groups = 'group.label', color='texture', label='label', name='texture', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')


groupedProfilePlot(g, groups = 'group.label', color='clay', label='label', name='clay', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='sand', label='label', name='sand', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='silt', label='label', name='silt', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='cec', label='label', name='cec', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='phh2o', label='label', name='phh2o', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')



plotSPC(subset(g, origin == 'SSURGO'), color='texture', label='label', name='texture', cex.names = 1, width=0.25, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')





