library(soilDB)
library(terra)

sg.crs <- crs('PROJCS["Homolosine", 
    GEOGCS["WGS 84", 
        DATUM["WGS_1984", 
            SPHEROID["WGS 84",6378137,298.257223563, 
                AUTHORITY["EPSG","7030"]], 
   AUTHORITY["EPSG","6326"]], 
        PRIMEM["Greenwich",0, 
            AUTHORITY["EPSG","8901"]], 
        UNIT["degree",0.0174532925199433, 
            AUTHORITY["EPSG","9122"]], 
        AUTHORITY["EPSG","4326"]], 
    PROJECTION["Interrupted_Goode_Homolosine"], 
    UNIT["Meter",1]]')


# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.88908,-120.47470,z13

# ca630
# table mountain and red hills
bb <- '-120.6393 37.8303,-120.6393 37.9554,-120.3602 37.9554,-120.3602 37.8303,-120.6393 37.8303'

# AOI
wkt <- sprintf('POLYGON((%s))', bb)
v <- vect(wkt, crs = 'EPSG:4326')

# standard layers
gssurgo <- mukey.wcs(v, db = 'gSSURGO')
gstatsgo <- mukey.wcs(v, db = 'statsgo')
issr800.clay <- ISSR800.wcs(v, var = 'clay_025cm')
issr800.pH <- ISSR800.wcs(v, var = 'pH_025cm')

# ISSR800 grid for reference
issr800.grid <- as.polygons(issr800.clay, values = 1, aggregate = FALSE)

# expand BBOX to EPSG 5070 for ISSR-800 WCS
v.bbox <- as.polygons(ext(gssurgo))
crs(v.bbox) <- 'EPSG:5070'

# expand BBOX to include large area, SoilGrids native CRS is highly skewed at this latitude
sg.bbox <- buffer(v.bbox, 5000)
crs(sg.bbox) <- 'EPSG:5070'

# vector MU delineations
ssurgo.poly <- SDA_spatialQuery(v.bbox, what = 'mupolygon', geomIntersection = TRUE, db = 'SSURGO')
ssurgo.poly <- project(ssurgo.poly, 'EPSG:5070')

statsgo.poly <- SDA_spatialQuery(v.bbox, what = 'mupolygon', geomIntersection = TRUE, db = 'STATSGO')
statsgo.poly <- project(statsgo.poly, 'EPSG:5070')

# soilgrids
# super slow
sg <- fetchSoilGrids(sg.bbox, grid = TRUE, variables = c('clay', 'phh2o'), depth_intervals = '5-15')
plot(sg)

# keep only the mean
sg.clay <- sg$`clay_mean_5-15cm`
sg.pH <- sg$`phh2o_mean_5-15cm`


# warp to same CRS
sg.clay <- project(sg.clay, 'epsg:5070', method = 'cubic')
sg.pH <- project(sg.pH, 'epsg:5070', method = 'cubic')


.cols <- hcl.colors(100, palette = 'zissou1')


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
  property = c('claytotal_r', 'ph1to1h2o_r'),
  method = 'Weighted Average', mukeys = unique(as.integer(ssurgo.poly$mukey)), 
  top_depth = 0, 
  bottom_depth = 25, 
  miscellaneous_areas = FALSE, 
  include_minors = TRUE
)

statsgo.tab <- get_SDA_property(
  property = c('claytotal_r', 'ph1to1h2o_r'),
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

gssurgo.pH <- as.numeric(gssurgo, index = 'ph1to1h2o_r')
gstatsgo.pH <- as.numeric(gstatsgo, index = 'ph1to1h2o_r')



clay.range <- round(
  range(
    c(
      range(ssurgo.rat$claytotal_r, na.rm = TRUE),
      range(statsgo.rat$claytotal_r, na.rm = TRUE), 
      range(values(issr800.clay, na.rm = TRUE)),
      range(values(sg.clay, na.rm = TRUE))
    )
  )
)

pH.range <- round(
  range(
    c(
      range(ssurgo.rat$ph1to1h2o_r, na.rm = TRUE),
      range(statsgo.rat$ph1to1h2o_r, na.rm = TRUE), 
      range(values(issr800.pH, na.rm = TRUE)),
      range(values(sg.pH, na.rm = TRUE))
    )
  )
)

# Red Hills, Table Mtn.
redhills <- ssurgo.poly[ssurgo.poly$mukey %in% c('1865927', '1865928', '1906347'), ]
tableMtn <- ssurgo.poly[ssurgo.poly$mukey %in% c('1865918', '2450845'), ]

redhills <- aggregate(redhills, dissolve = TRUE)
tableMtn <- aggregate(tableMtn, dissolve = TRUE)


par(mfcol = c(2, 2))

plot(gssurgo.clay, axes = FALSE, main = 'gSSURGO Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = clay.range, ext = gssurgo.clay)
polys(redhills, lwd = 0.5, border = 'black')
polys(tableMtn, lwd = 0.5, border = 'black')

plot(gstatsgo.clay, axes = FALSE, main = 'STATSGO Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = clay.range, ext = gssurgo.clay)
polys(redhills, lwd = 0.5, border = 'black')
polys(tableMtn, lwd = 0.5, border = 'black')

plot(issr800.clay, axes = FALSE, main = 'ISSR800 Clay 0-25cm', mar = c(1, 1, 1, 4), col = .cols, range = clay.range, ext = gssurgo.clay)
polys(redhills, lwd = 0.5, border = 'black')
polys(tableMtn, lwd = 0.5, border = 'black')


plot(sg.clay, axes = FALSE, main = 'SoilGrids 250m Clay 5-15cm', mar = c(1, 1, 1, 4), col = .cols, range = clay.range, ext = gssurgo.clay)
polys(redhills, lwd = 0.5, border = 'black')
polys(tableMtn, lwd = 0.5, border = 'black')




par(mfcol = c(2, 2))

plot(gssurgo.pH, axes = FALSE, main = 'gSSURGO pH 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = pH.range, ext = gssurgo.pH)
polys(redhills, lwd = 0.5, border = 'black')
# polys(tableMtn, lwd = 0.5, border = 'black')

plot(gstatsgo.pH, axes = FALSE, main = 'STATSGO pH 0-25cm', mar = c(1, 1, 1, 4), col = .cols, type = 'continuous', range = pH.range, ext = gssurgo.pH)
polys(redhills, lwd = 0.5, border = 'black')
# polys(tableMtn, lwd = 0.5, border = 'black')

plot(issr800.pH, axes = FALSE, main = 'ISSR800 pH 0-25cm', mar = c(1, 1, 1, 4), col = .cols, range = pH.range, ext = gssurgo.pH)
polys(redhills, lwd = 0.5, border = 'black')
# polys(tableMtn, lwd = 0.5, border = 'black')

plot(sg.pH, axes = FALSE, main = 'SoilGrids 250m pH 5-15cm', mar = c(1, 1, 1, 4), col = .cols, range = pH.range, ext = gssurgo.pH)
polys(redhills, lwd = 0.5, border = 'black')
# polys(tableMtn, lwd = 0.5, border = 'black')







par(mfcol = c(1, 2))

x <- resample(sg.clay, gssurgo.clay, method = 'cubic')
plot(gssurgo.clay - x, col = hcl.colors(100, palette = 'spectral'), axes = FALSE, main = 'SSURGO - SoilGrids 250\n Clay %')
# polys(redhills, lwd = 0.5, border = 'black')

x <- resample(sg.pH, gssurgo.pH, method = 'cubic')
plot(gssurgo.pH - x, col = hcl.colors(100, palette = 'spectral'), axes = FALSE, main = 'SSURGO - SoilGrids 250\npH')
# polys(redhills, lwd = 0.5, border = 'black')


# pH
knitr::kable(
  data.frame(
    SSURGO = global(gssurgo.pH, mean, na.rm = TRUE)[1, ],
    STASTGO = global(gstatsgo.pH, mean, na.rm = TRUE)[1, ],
    ISSR800 = global(issr800.pH, mean, na.rm = TRUE)[1, ], 
    SoilGrids = global(sg.pH, mean, na.rm = TRUE)[1, ]
  ), digits = 2, caption = 'Global Mean pH 1:1 Soil:Water'
)


# clay
knitr::kable(
  data.frame(
    SSURGO = global(gssurgo.clay, mean, na.rm = TRUE)[1, ],
    STASTGO = global(gstatsgo.clay, mean, na.rm = TRUE)[1, ],
    ISSR800 = global(issr800.clay, mean, na.rm = TRUE)[1, ], 
    SoilGrids = global(sg.clay, mean, na.rm = TRUE)[1, ]
  ), digits = 2, caption = 'Global Mean Percent Clay'
)



