library(raster)
library(rgdal)
library(plyr)

# load legend
m <- read.table('mapping.txt', stringsAsFactors=FALSE)
names(m) <- c('code', 'compname')

# get base set of netCDF file names
files <- list.files(path='tiff/', pattern='*prob_1\\.tif$')

# list to store details
l <- list()

# iterate over these names and do some comparisons
for(i in seq_along(files)) {
  
  f.i <- files[i]
  print(f.i)
  
  # strip out end of file name
  f.i <- gsub('_prob_1.tif', '', f.i)
  
  # load rasters
  r.class.1 <- raster(paste0('tiff/', f.i, '_class_1.tif'))
  r.prob.1 <- raster(paste0('tiff/', f.i, '_prob_1.tif'))
  r.class.2 <- raster(paste0('tiff/', f.i, '_class_2.tif'))
  r.prob.2 <- raster(paste0('tiff/', f.i, '_prob_2.tif'))
  r.class.3 <- raster(paste0('tiff/', f.i, '_class_3.tif'))
  r.prob.3 <- raster(paste0('tiff/', f.i, '_prob_3.tif'))
  
  # set the nodata value (-9999)
  NAvalue(r.class.1) <- -9999
  NAvalue(r.prob.1) <- -9999
  NAvalue(r.class.2) <- -9999
  NAvalue(r.prob.2) <- -9999
  NAvalue(r.class.3) <- -9999
  NAvalue(r.prob.3) <- -9999
  
  # stack class rasters
  s.1 <- stack(r.class.1, r.prob.1)
  names(s.1) <- c('code', 'prob')
  
  s.2 <- stack(r.class.2, r.prob.2)
  names(s.2) <- c('code', 'prob')
  
  s.3 <- stack(r.class.3, r.prob.3)
  names(s.3) <- c('code', 'prob')
  
  # sample first stack along a regular grid and save as SPDF
  ss.1 <- sampleRegular(s.1, size=250, sp=TRUE)
  
  # sample 2nd and 3rd stacks
  # use spatial object details from first stack
  ss.2 <- ss.1
  ss.3 <- ss.1
  ss.2@data <- data.frame(extract(s.2, ss.1))
  ss.3@data <- data.frame(extract(s.3, ss.1))
  
  # join legend
  ss.1@data <- join(ss.1@data, m, by='code', type='left')
  ss.2@data <- join(ss.2@data, m, by='code', type='left')
  ss.3@data <- join(ss.3@data, m, by='code', type='left')
  
  # save ML class number
  ss.1$class <- 1
  ss.2$class <- 2
  ss.3$class <- 3
  
  # save results
  l[[i]] <- rbind(ss.1, ss.2, ss.3)
}

# convert to DF
d <- do.call('rbind', l)

# save SHP file for postGIS analysis
writeOGR(d, dsn='points-for-ssurgo-query', layer='samples', driver='ESRI Shapefile', overwrite_layer=TRUE)


