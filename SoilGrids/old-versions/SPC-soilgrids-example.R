library(aqp)
library(soilDB)
library(jsonlite)
library(reshape2)
library(digest)
library(lattice)
library(tactile)
library(cluster)
library(sharpshootR)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.98821,-120.39788,z15

source('local-functions.R')


## TODO: clean this up!

x <- c(-120.39788, -120.43179, -119.69656, -119.69132, -119.72291, -119.71980, -119.68240, -120.81725, -120.81219)
y <- c(37.98821, 37.97692, 36.73266, 36.72908, 36.90112, 36.90239, 36.91291, 38.24330, 38.24169)
desc <- c('Sonora 1', 'Sonora 2', 'DhA', 'Ex', 'Ra', 'SdA', 'ChC', 'MVO', 'SER')

coords <- data.frame(x, y, desc, stringsAsFactors = FALSE)


g <- prepData(x = coords[1, 1], y = coords[1, 2], gl = coords$desc[1])

par(mar=c(0,0,3,1))
plotSPC(g, color='clay', label='label', name='hzname')
plotSPC(g, color='sand', label='label', name='hzname')
plotSPC(g, color='silt', label='label', name='hzname')
plotSPC(g, color='phh2o', label='label', name='hzname')
plotSPC(g, color='cec', label='label', name='hzname')

groupedProfilePlot(g, groups = 'origin', color='clay', label='label', name='hzname', group.name.offset = -15)


groupedProfilePlot(g, groups = 'unique.id', color='clay', label='label', name='hzname', group.name.offset = -15)

groupedProfilePlot(g, groups = 'group.label', color='clay', label='label', name='hzname', group.name.offset = -15)


# CA654: Ramona / San Joaquin / Centerville
g <- lapply(5:7, function(i) {
  prepData(x = coords[i, 1], y = coords[i, 2], gl = coords$desc[i])
})

# CA630: MVO / SER interface
g <- lapply(8:9, function(i) {
  prepData(x = coords[i, 1], y = coords[i, 2], gl = coords$desc[i])
})

g <- combine(g)

# sanity check on sand + silt + clay
g$ssc <- g$sand + g$silt + g$clay

# par(mar=c(0,0,3,1))
# plotSPC(g, color='clay', label='label', name='hzname', id.style='side')
# 
# groupedProfilePlot(g, groups = 'origin', color='clay', label='label', name='hzname', group.name.offset = -15, id.style='side')
# 
# 
# groupedProfilePlot(g, groups = 'unique.id', color='clay', label='label', name='hzname', group.name.offset = -15, id.style='side')
# 
# groupedProfilePlot(g, groups = 'group.label', color='clay', label='label', name='hzname', group.name.offset = -15)


par(mar=c(0,0,3,0))
groupedProfilePlot(g, groups = 'group.label', color='sand', label='label', name='hzname', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='clay', label='label', name='hzname', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')


groupedProfilePlot(g, groups = 'group.label', color='clay', label='label', name='clay', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='silt', label='label', name='silt', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='ssc', label='label', name='ssc', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='sand', label='label', name='sand', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='cec', label='label', name='cec', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')

groupedProfilePlot(g, groups = 'group.label', color='phh2o', label='label', name='phh2o', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')




groupedProfilePlot(filter(g, group.label %in% c('MVO')), groups = 'group.label', color='clay', label='label', name='clay', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')


## not so bad when reducing to an aggregate depth interval wt. mena
## how about smashing the entire collection to a depth interval?

## sadly slab() cannot use comp weights

# be sure to remove upr/lwr
slab(filter(g, group.label == 'MVO' & !grepl('lwr|upr', label)), origin ~ clay + sand + silt + cec, slab.structure = c(0, 25), slab.fun = mean, na.rm=TRUE)




## look at covariance by origin






## look at variation
## note: sometimes includes multiple components

g <- lapply(1:nrow(coords), function(i) {
  prepData(x = coords[i, 1], y = coords[i, 2], gl = coords$desc[i])
})

g <- combine(g)

groupedProfilePlot(subset(g, group.label == 'Sonora 1'), groups = 'group.label', color='phh2o', label='label', name='clay', group.name.offset = -15, cex.names = 0.75, width=0.3, plot.depth.axis=FALSE, hz.depths=TRUE, name.style='center-center')


g.sub <- subset(g, group.label == 'Sonora 1')

d <- profile_compare(g.sub, vars = c('clay', 'sand', 'cec'), k = 0, max_d = 180)

plotProfileDendrogram(g.sub, diana(d), scaling.factor = 1.1, y.offset = 20, color='clay', label='label', name='clay', group.name.offset = -15, cex.names = 0.75, width=0.3)


d <- as(g, 'data.frame')

# remove upper / lower SG values
d <- subset(d, subset=!grepl('lwr|upr', label))


bwplot(clay ~ origin | group.label, data=d, par.settings = tactile.theme(), scales=list(y=list(relation='free', alternating=3, rot=0)))

bwplot(sand ~ origin | group.label, data=d, par.settings = tactile.theme(), scales=list(y=list(relation='free', alternating=3, rot=0)))

bwplot(cec ~ origin | group.label, data=d, par.settings = tactile.theme(), scales=list(y=list(relation='free', alternating=3, rot=0)))

bwplot(phh2o ~ origin | group.label, data=d, par.settings = tactile.theme(), scales=list(y=list(relation='free', alternating=3, rot=0)))
