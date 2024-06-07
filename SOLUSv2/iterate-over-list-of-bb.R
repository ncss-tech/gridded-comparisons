
## get the latest rgeedim
# remotes::install_github("brownag/rgeedim")

library(rgeedim)
library(soilDB)
library(aqp)
library(terra)
library(viridis)
library(rasterVis)
library(latticeExtra)
library(tactile)
library(purrr)


source('local-functions.R')

# soil texture color table
txt.lut <- read.csv('texture-color-table.csv')

# this should bring up a web page with steps to setup temporary auth
# use the default EE project
# TODO: document this!

# use as needed
# gd_authenticate(auth_mode = "notebook")

# init python back-end
gd_initialize()

## list to iterate over
source('JMS-list.R')
out.dir <- file.path('requests', 'JMS')
dir.create(file.path(out.dir, 'figures'), recursive = TRUE)
dir.create(file.path(out.dir, 'export'), recursive = TRUE)

source('misc-list.R')
out.dir <- file.path('requests', 'misc')
dir.create(file.path(out.dir, 'figures'), recursive = TRUE)
dir.create(file.path(out.dir, 'export'), recursive = TRUE)

# iterate
.processBB <- function(i) {
  
  # create WKT from SoilWeb style BBOX
  bb <- sprintf("POLYGON((%s))", i$bb)
  bb <- vect(bb, crs = "OGC:CRS84")
  
  .a <- abbreviate(i$caption, minlength = 12)
  .a <- gsub(pattern = ',', replacement = '', x = .a, fixed = TRUE)
  
  .im <- file.path(out.dir, 'figures', sprintf("%s.png", .a))
  
  ragg::agg_png(filename = .im, width = 2200, height = 1200, scaling = 2)
  
  z <- compareAOI(bb, figTitle = i$caption)
  
  dev.off()
  
  
  ## save results
  .f <- file.path(out.dir, 'export', sprintf('%s-gnatsgo-clay-15cm.tif', .a))
  writeRaster(z$clay$gNATSGO, filename = .f, overwrite = TRUE)
  
  .f <- file.path(out.dir, 'export', sprintf('%s-solusv2-clay-15cm.tif', .a))
  writeRaster(z$clay$SOLUS, filename = .f, overwrite = TRUE)
  
  .f <- file.path(out.dir, 'export', sprintf('%s-gnatsgo-sand-15cm.tif', .a))
  writeRaster(z$sand$gNATSGO, filename = .f, overwrite = TRUE)
  
  .f <- file.path(out.dir, 'export', sprintf('%s-solusv2-sand-15cm.tif', .a))
  writeRaster(z$sand$SOLUS, filename = .f, overwrite = TRUE)
  
  .f <- file.path(out.dir, 'export', sprintf('%s-gnatsgo-texture-15cm.tif', .a))
  writeRaster(z$gNATSGO.texture, filename = .f, overwrite = TRUE)
  
  .f <- file.path(out.dir, 'export', sprintf('%s-solusv2-texture-15cm.tif', .a))
  writeRaster(z$SOLUS.texture, filename = .f, overwrite = TRUE)
  
  return(NULL)
}



walk(x, .f = .processBB, .progress = TRUE)

write.csv(
  do.call('rbind', lapply(x, data.frame)),
  file = file.path(out.dir, 'manifest.txt'),
  row.names = FALSE
)





