library(soilDB)
library(terra)


## where does the soil data come from, if missing in most of STATSGO?


get_SDA_property(
  property = 'claytotal_r',
  method = 'Weighted Average', mukeys = 1678526, 
  top_depth = 0, 
  bottom_depth = 25, 
  miscellaneous_areas = FALSE, 
  include_minors = TRUE
)


x <- fetchSDA_spatial(x = 1678526)
x <- vect(x)

plot(x)

v <- x




# https://casoilresource.lawr.ucdavis.edu/soil-properties/?prop=survey_type&lat=35.3599&lon=-112.4698&z=9
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=35.36512,-112.44473,z13

# az695
bb <- '-112.5553 35.2983,-112.5553 35.4277,-112.3314 35.4277,-112.3314 35.2983,-112.5553 35.2983'


# ca666
bb <- '-119.0001 35.3734,-119.0001 35.5026,-118.7763 35.5026,-118.7763 35.3734,-119.0001 35.3734'

# latest test from Chad
bb <- '-111.3820 33.7662,-111.3820 33.9030,-111.0524 33.9030,-111.0524 33.7662,-111.3820 33.7662'

# ca630
bb <- '-120.4740 37.9809,-120.4740 38.0455,-120.3545 38.0455,-120.3545 37.9809,-120.4740 37.9809'


wkt <- sprintf('POLYGON((%s))', bb)
v <- vect(wkt, crs = 'EPSG:4326')



## latest from Chad
# e <- ext(-112.06532953734000557, -110.5266330541149955, 33.17770965763599378, 34.45346418563027413)
# v <- vect(e, crs = 'EPSG:4326')





gssurgo <- mukey.wcs(v, db = 'gSSURGO')
gstatsgo <- mukey.wcs(v, db = 'statsgo')
issr800.clay <- ISSR800.wcs(v, var = 'clay_025cm')


issr800.grid <- as.polygons(issr800.clay, values = 1, aggregate = FALSE)



v.bbox <- as.polygons(ext(gssurgo))
crs(v.bbox) <- 'EPSG:5070'

ssurgo.poly <- SDA_spatialQuery(v.bbox, what = 'mupolygon', geomIntersection = TRUE, db = 'SSURGO')
ssurgo.poly <- project(ssurgo.poly, 'EPSG:5070')

statsgo.poly <- SDA_spatialQuery(v.bbox, what = 'mupolygon', geomIntersection = TRUE, db = 'STATSGO')
statsgo.poly <- project(statsgo.poly, 'EPSG:5070')


par(mfcol = c(1, 3))

.cols <- hcl.colors(100, palette = 'zissou1')

plot(gssurgo, axes = FALSE, main = 'gSSURGO mukey', mar = c(1, 1, 1, 4), col = .cols)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)

plot(gstatsgo, axes = FALSE, main = 'STATSGO mukey', mar = c(1, 1, 1, 4), col = .cols)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)


plot(issr800.clay, axes = FALSE, main = 'ISSR800 Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols)
lines(issr800.grid, lwd = 0.25)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)




## check mukey alignment between gridded / conventional
ssurgo.rat <- cats(gssurgo)[[1]]
statsgo.rat <- cats(gstatsgo)[[1]]

# all the same!
setdiff(ssurgo.rat$mukey, ssurgo.poly$mukey)
setdiff(ssurgo.poly$mukey, ssurgo.rat$mukey)

setdiff(statsgo.rat$mukey, statsgo.poly$mukey)
setdiff(statsgo.poly$mukey, statsgo.rat$mukey)


## get aggregate soil property values
ssurgo.tab <- get_SDA_property(
  property = 'claytotal_r',
  method = 'Weighted Average', mukeys = unique(as.integer(ssurgo.poly$mukey)), 
  top_depth = 0, 
  bottom_depth = 25, 
  miscellaneous_areas = FALSE, 
  include_minors = TRUE
)

statsgo.tab <- get_SDA_property(
  property = 'claytotal_r',
  method = 'Weighted Average', mukeys = unique(as.integer(statsgo.poly$mukey)), 
  top_depth = 0, 
  bottom_depth = 25, 
  miscellaneous_areas = FALSE, 
  include_minors = TRUE
)


## join RAT -> flatten
ssurgo.rat <- merge(ssurgo.rat, ssurgo.tab, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

statsgo.rat <- merge(statsgo.rat, statsgo.tab, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

ssurgo.rat$mukey <- as.integer(ssurgo.rat$mukey)
statsgo.rat$mukey <- as.integer(statsgo.rat$mukey)

levels(gssurgo) <- ssurgo.rat
levels(gstatsgo) <- statsgo.rat

gssurgo.clay <- as.numeric(gssurgo, index = 'claytotal_r')
gstatsgo.clay <- as.numeric(gstatsgo, index = 'claytotal_r')


# full range
.range <- round(
  range(
    c(
      ssurgo.rat$claytotal_r, 
      statsgo.rat$claytotal_r,
      values(issr800.clay)
    ),
    na.rm = TRUE
  )
)





par(mfcol = c(1, 3))

plot(gssurgo.clay, axes = FALSE, main = 'gSSURGO Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = .range)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)

plot(gstatsgo.clay, axes = FALSE, main = 'STATSGO Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = .range)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)


plot(issr800.clay, axes = FALSE, main = 'ISSR800 Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, range = .range)
lines(issr800.grid, lwd = 0.25)
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)




par(mfcol = c(1, 2))


x <- resample(issr800.clay, gssurgo.clay, method = 'near')
plot(x - gssurgo.clay, col = hcl.colors(100, palette = 'spectral'), axes = FALSE, main = 'ISSR800 - SSURGO')
lines(issr800.grid, lwd = 0.25, col = grey(0.5))
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)



x <- resample(issr800.clay, gstatsgo.clay, method = 'near')
plot(x - gstatsgo.clay, col = hcl.colors(100, palette = 'spectral'), axes = FALSE, main = 'ISSR800 - STATSGO')
lines(issr800.grid, lwd = 0.25, col = grey(0.5))
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)


par(mfcol = c(1, 1))

x <- resample(gstatsgo.clay, gssurgo.clay, method = 'cubic')
plot(x - gssurgo.clay, col = hcl.colors(100, palette = 'spectral'), axes = FALSE, main = 'STATSGO - SSURGO')
lines(ssurgo.poly, lwd = 0.5)
lines(statsgo.poly, lwd = 2)







