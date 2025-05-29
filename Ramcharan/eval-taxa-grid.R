library(soilDB)
library(terra)
library(lattice)
library(tactile)
library(hexbin)

sg <- rast('e:/temp/jq237hs159_version1_TAXgg_Calciargids_100m.tif')

tg <- taxaExtent('calciargids', level = 'greatgroup')


plot(sg > 25)
plot(tg > 25, add = TRUE, col = c('grey', 'red'))



s <- spatSample(tg, size = 1e5, na.rm = TRUE, as.points = TRUE)
s$sg <- extract(sg, s)[, 2]

s <- as.data.frame(s)
head(s, 50)

xyplot(calciargids ~ sg, data = s, par.settings = tactile.theme(), subset = sg > 0 & calciargids > 0)


hexbinplot(
  calciargids ~ sg, 
  data = s, 
  par.settings = tactile.theme(), 
  subset = sg > 25 & calciargids > 25,
  asp = 1, 
  trans = log,
  inv = exp,
  colramp = hcl.colors, 
  colorkey = FALSE
)
