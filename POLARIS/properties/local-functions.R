

## make links to POLARIS soil property raster tiles
# requires raster package
# http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/Readme
# 
# r: raster object with defined CRS
# property: 
# depth: 
generateLinks <- function(r, property='ph', depth='5_15') {
  
  # generate extent in GCS WGS84
  e <- extent(projectExtent(r300, CRS('+proj=longlat +datum=WGS84')))
  # convert extent into vector
  # xmin, xmax, ymin, ymax
  e <- as(e, 'vector')
  
  # expand sequence to bounding integers 
  dx <- seq(from=floor(e[1]), to=ceiling(e[2]), by = 1)
  dy <- seq(from=floor(e[3]), to=ceiling(e[4]), by = 1)
  
  # generate entire sequence of tiles
  tiles <- expand.grid(x=dx, y=dy)
  
  # file names
  f <- sprintf('lat%s%s_lon%s%s.tif', tiles$y, tiles$y+1, tiles$x, tiles$x+1)
  
  # this is the target soil property/statistic/depth
  u <- sprintf('http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/%s/mean/%s/%s', property, depth, f)
  
  return(u)
}





