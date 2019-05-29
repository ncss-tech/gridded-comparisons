library(raster)
library(rgdal)
library(plyr)

# get base set of netCDF file names
files <- list.files(path='.', pattern='*\\.tif$')

# list to store details
l <- list()

# iterate over these names and do some comparisons
for(i in seq_along(files)) {
  
  f.i <- files[i]
  print(f.i)
    
  # load rasters
  r <- raster(f.i)
  
  # set the nodata value (0)
  NAvalue(r) <- 0
  
  # get RAT
  rat <- levels(r)[[1]]
  
  # sample first stack along a regular grid and save as SPDF
  s <- sampleRegular(r, size=10000, sp=TRUE)
  
  # re-name for joining
  names(s) <- 'ID'
  
  # join with RAT
  s@data <- join(s@data, rat, by='ID', type='left')
  
  # save results
  l[[i]] <- s
}

# convert to DF
d <- do.call('rbind', l)

# transform to NAD83 GCS
d <- spTransform(d, CRS('+proj=longlat +datum=NAD83'))

# save SHP file for postGIS analysis
writeOGR(d, dsn='.', layer='samples', driver='ESRI Shapefile', overwrite_layer=TRUE)


