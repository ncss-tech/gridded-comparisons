library(aqp)
library(soilDB)
library(terra)


# CA654 -- Biglione Ranch
b <- 'POLYGON((-119.7744 36.8975,-119.7744 36.9629,-119.6553 36.9629,-119.6553 36.8975,-119.7744 36.8975))'

# OK153
# https://soilmap4-1.lawr.ucdavis.edu/gmap/?loc=36.78069,-99.14955,z12
# b <- 'POLYGON((-99.2798 36.7038,-99.2798 36.8484,-99.0392 36.8484,-99.0392 36.7038,-99.2798 36.7038))'


# IL113: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=40.48257,-88.50041,z15
b <- 'POLYGON((-88.5369 40.4675,-88.5369 40.4987,-88.4682 40.4987,-88.4682 40.4675,-88.5369 40.4675))'

# Davis, CA
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.54333,-121.74929,z13
b <- 'POLYGON((-121.8849 38.4787,-121.8849 38.6068,-121.6101 38.6068,-121.6101 38.4787,-121.8849 38.4787))'


# Keith county, NE
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=41.24103,-101.63864,z13
b <- 'POLYGON((-101.7703 41.1811,-101.7703 41.3042,-101.4972 41.3042,-101.4972 41.1811,-101.7703 41.1811))'


b <- vect(b, crs = 'epsg:4326')

b <- project(b, 'epsg:5070')
b <- as.polygons(ext(b), crs = 'epsg:5070')
plot(b)

s <- soilDB::SDA_spatialQuery(
  b,
  what = 'mupolygon',
  db = 'SSURGO',
  geomIntersection = TRUE
)

s <- project(s, 'epsg:5070')

r <- fetchSOLUS(
  b,
  depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
  variables = c("sandtotal", "claytotal", "cec7", "resdept"),
  output_type = "prediction",
  grid = TRUE
)

plot(r[['sandtotal_60_cm_p']])
lines(s)

z <- r[[1]]
values(z) <- ssc_to_texcl(sand = values(r$sandtotal_30_cm_p), clay = values(r$claytotal_30_cm_p), droplevels = FALSE, simplify = TRUE)

.ct <- soilTextureColorPal(simplify = TRUE)

.ct <- merge(
  levels(z)[[1]],
  .ct, 
  by.x = 'label',
  by.y = 'class',
  sort = FALSE,
  all.x = TRUE
)

coltab(z) <- .ct[, c('value', 'color')]

plot(z)
lines(s)


plot(r$resdept_all_cm_p)
lines(s)

plot(r$claytotal_5_cm_p)
lines(s)

plot(r$sandtotal_5_cm_p)
lines(s)


sg <- fetchSoilGrids(b, grid = TRUE, variables = c('sand', 'clay'), depth_intervals = '15-30', summary_type = 'mean')

sg.z <- sg$`sand_mean_15-30cm`

values(sg.z) <- ssc_to_texcl(
  sand = values(sg$`sand_mean_15-30cm`), 
  clay = values(sg$`clay_mean_15-30cm`), 
  droplevels = FALSE, 
  simplify = TRUE
)

coltab(sg.z) <- .ct[, c('value', 'color')]

plot(sg.z)


mu <- mukey.wcs(aoi = b, db = 'gssurgo')

statsgo <- soilDB::SDA_spatialQuery(
  b,
  what = 'mupolygon',
  db = 'STATSGO',
  geomIntersection = FALSE
)


statsgo <- project(statsgo, 'epsg:5070')

statsgo <- rasterize(statsgo, mu, field = 'mukey')
statsgo <- as.factor(statsgo)

plot(statsgo)

# extract RAT for thematic mapping
rat <- cats(statsgo)[[1]]

# variables of interest
vars <- c("sandtotal_r", "claytotal_r", "cec7_r")

# get / aggregate specific horizon-level properties from SDA
# be sure to see the manual page for this function
p <-  get_SDA_property(property = vars,
                       method = "weighted average", 
                       mukeys = as.integer(rat$mukey),
                       include_minors = TRUE, 
                       miscellaneous_areas = FALSE,
                       top_depth = 29,
                       bottom_depth = 30)


# merge aggregate soil data into RAT
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# requires that grid cell ID (mukey) be numeric
rat$mukey <- as.integer(rat$mukey)
levels(statsgo) <- rat


statsgo.stack <- catalyze(statsgo)


statsgo.txt <- statsgo
values(statsgo.txt) <- ssc_to_texcl(sand = values(statsgo.stack$sandtotal_r), clay = values(statsgo.stack$claytotal_r), droplevels = FALSE, simplify = TRUE)

coltab(statsgo.txt) <- .ct[, c('value', 'color')]



# extract RAT for thematic mapping
rat <- cats(mu)[[1]]

# variables of interest
vars <- c("sandtotal_r", "claytotal_r", "cec7_r")

# get / aggregate specific horizon-level properties from SDA
# be sure to see the manual page for this function
p <-  get_SDA_property(property = vars,
                       method = "weighted average", 
                       mukeys = as.integer(rat$mukey),
                       include_minors = TRUE, 
                       miscellaneous_areas = FALSE,
                       top_depth = 29,
                       bottom_depth = 30)


# merge aggregate soil data into RAT
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# requires that grid cell ID (mukey) be numeric
rat$mukey <- as.integer(rat$mukey)
levels(mu) <- rat

activeCat(mu) <- 'claytotal_r'
plot(mu)

mu.stack <- catalyze(mu)

mu.txt <- mu
values(mu.txt) <- ssc_to_texcl(sand = values(mu.stack$sandtotal_r), clay = values(mu.stack$claytotal_r), droplevels = FALSE, simplify = TRUE)

coltab(mu.txt) <- .ct[, c('value', 'color')]


issr <- ISSR800.wcs(b, var = 'texture_2550cm')

sg.z <- project(sg.z, mu.txt)


## ramcharan data here
# https://scholarsphere.psu.edu/resources/a4db13d9-7839-40f4-9885-34021a38f106
rm <- rast(c('e:/gis_data/ramcharan/clay_M_sl4_100m.tif', 'e:/gis_data/ramcharan/sand_M_sl4_100m.tif'))
# crop / warp
rm <- project(rm, mu.txt)

rm.txt <- rm$clay_M_sl4_100m
values(rm.txt) <- ssc_to_texcl(sand = values(rm$sand_M_sl4_100m), clay = values(rm$clay_M_sl4_100m), droplevels = FALSE, simplify = TRUE)
coltab(rm.txt) <- .ct[, c('value', 'color')]

# bleh
plot(rm.txt)
lines(s)


g <- c(
  mu.txt, 
  statsgo.txt, 
  resample(issr, mu.txt, method = 'near'),
  resample(z, mu.txt, method = 'near'),
  sg.z,
  rm.txt
)

names(g) <- c('SSURGO (29-30cm) | 1:20k', 'STATSGO (29-30cm) | 1:250k', 'ISSR800 (25-50cm) | 1:300k', 'SOLUS 100m (30cm) | 1:100k', 'SoilGrids 250m (15-30cm) | ?', 'Ramcharam et al. 100m (30cm) | ?')

plot(g, axes = FALSE, mar = c(0.5, 0.5, 2, 0), fun = function() lines(s), plg = list(cex = 1.5))

plot(g, axes = FALSE, mar = c(0.5, 0.5, 2, 0), plg = list(cex = 1.5))

# ragg::agg_png(file = 'e:/temp/gridded-comparison-ranch.png', width = 2900, height = 1500, scaling = 2)

# plot(g, axes = FALSE, mar = c(0.5, 0.5, 2, 0), plg = list(cex = 1.5), legend = FALSE, fun = function() lines(s))

plot(g, axes = FALSE, plg = list(cex = 1.5), legend = TRUE, fun = function() lines(s))

# dev.off()


plot(g[[c(1, 4, 5, 6)]], axes = FALSE, mar = c(0.5, 0.5, 2, 0), plg = list(cex = 1.5))





##

# http://127.0.0.1:41463/graphics/plot_zoom_png?width=1614&height=861
plot(mu.stack$sandtotal_r)
plot(r$sandtotal_30_cm_p)
plot(rm$sand_M_sl4_100m)

.diff <- mu.stack$sandtotal_r - resample(r$sandtotal_30_cm_p, mu.stack$sandtotal_r)


plot(scale(.diff), col = hcl.colors(n = 50, palette = 'vik'))

a <- autocor(.diff, method = "moran", global = FALSE)
plot(a, axes = FALSE, col = hcl.colors(n = 50, palette = 'vik'))


library(geodiv)

.w = matrix(1, nrow = 5, ncol = 5)

sa.ssurgo <- focal_metrics(mu.stack$sandtotal_r, window = .w, metric = "sa", progress = FALSE, na.rm = TRUE)

sa.solus <- focal_metrics(resample(r$sandtotal_30_cm_p, mu.stack$sandtotal_r), window = .w, metric = "sa", progress = FALSE)

.diff <- sa.ssurgo$sa - sa.solus$sa

plot(.diff, col = hcl.colors(n = 50, palette = 'vik'))



##

library(diffeR)

.mad = MAD(mu.stack$sandtotal_r, resample(r$sandtotal_30_cm_p, mu.stack$sandtotal_r), eval = "multiple")
.mad

categoryComponentsPlot(comp = mu.txt, ref = resample(z, mu.txt, method = 'near'))


plot(memberships(mu.txt, fact = 4))

ctmat <- crosstabm(comp = mu.txt, ref = resample(z, mu.txt, method = 'near'), percent = TRUE)
overallComponentsPlot(ctmatrix = ctmat, units = "percent")


overallSourcesPlot(comp = mu.txt, ref = resample(z, mu.txt, method = 'near'), analysis = 'error')
overallSourcesPlot(comp = mu.txt, ref = resample(z, mu.txt, method = 'near'), analysis = 'change')



##
plot(r$resdept_all_cm_p)
lines(s)



# SoilProfileCollection output, using linear interpolation for 1cm slices
# site-level variables (e.g. resdept) added to site data.frame of SPC
x <- fetchSOLUS(
  s,
  depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
  variables = c("sandtotal", "silttotal", "claytotal", "cec7", "resdept"),
  output_type = "prediction",
  method = "linear",
  grid = FALSE,
  samples = 10
)

x$tex <- ssc_to_texcl(sand = x$sandtotal_p, clay = x$claytotal_p)

par(mar = c(0, 0, 3, 3))
plotSPC(x, color = "claytotal_p", divide.hz = FALSE, width = 0.35, cex.names = 0.85)
abline(h = c(5, 15, 30, 60, 100), lty = 3)

plotSPC(x, color = "tex", divide.hz = FALSE, width = 0.35, cex.names = 0.85)

plotSPC(x, color = "cec7_p", divide.hz = FALSE, width = 0.35, cex.names = 0.85)


