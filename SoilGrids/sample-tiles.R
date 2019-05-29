
# get base set of netCDF file names
files <- list.files(path='tiff/', pattern='*original\\.tif$')

# list to store details
l <- list()
l.data <- list()

# iterate over these names and do some comparisons
for(i in seq_along(files)) {
  
  f.i <- files[i]
  print(f.i)
  
  # strip out end of file name
  f.i <- gsub('_original.tif', '', f.i)
  
  # extract coordinates
  y0 <- as.integer(substr(f.i, 4, 5))
  y1 <- as.integer(substr(f.i, 6, 7))
  
  x <- gsub('lon', '', sapply(strsplit(f.i, split='_'), function(j) j[[2]]))
  x0 <- -as.integer(sapply(strsplit(x, split='-'), function(j) j[[2]]))
  x1 <- -as.integer(sapply(strsplit(x, split='-'), function(j) j[[3]]))
  
  # load class rasters
  r.class.1 <- raster(paste0('tiff/', f.i, '_class_1.tif'))
  #   r.prob.1 <- raster(paste0('tiff/', f.i, '_prob_1.tif'))
  r.original <- raster(paste0('tiff/', f.i, '_original.tif'))
  
  # set the nodata value (-9999)
  NAvalue(r.class.1) <- -9999
  #   NAvalue(r.prob.1) <- -9999
  NAvalue(r.original) <- -9999
  
  # stack class rasters
  s <- stack(r.class.1, r.original)
  names(s) <- c('ml_class_1', 'original')
  
  # sample
  ss <- sampleRegular(s, size=10000)
  
  # levels of "mapped" are the gold standard, anything outside of these values isn't helpful
  mapped <- factor(ss[, 2])
  # force levels to those of "mapped", all other values set to NA
  predicted.1 <- factor(ss[, 1], levels=levels(mapped))
  
  # this is the correct comparison
  ca.1 <- unlist(classAgreement(table(mapped, predicted.1)))
  
  # re-name
  ca.names <- c("diag", "kappa", "rand", "crand")
  names(ca.1) <- paste0(ca.names, '.1')
  
  # save results
  l[[i]] <- data.frame(cbind(t(ca.1), x0, y0, x1, y1))
  l.data[[f.i]] <- ss
}

# convert to DF
d <- ldply(l)
d.data <- ldply(l.data)

# save samples for next time
save(d, d.data, file='samples.rda')