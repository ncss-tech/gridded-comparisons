# https://gitlab.com/openlandmap/global-layers/-/blob/master/tutorial/OpenLandMap_COG_tutorial.md

# https://gitlab.com/openlandmap/global-layers/-/blob/master/tables/openlandmap_wasabi_files.csv

# predicted250m;sol_texture.class_usda.tt_m_250m_b0..0cm_1950..2017_v0.2.tif;345MiB
# predicted250m;sol_texture.class_usda.tt_m_250m_b10..10cm_1950..2017_v0.2.tif;350MiB
# predicted250m;sol_texture.class_usda.tt_m_250m_b100..100cm_1950..2017_v0.2.tif;385MiB
# predicted250m;sol_texture.class_usda.tt_m_250m_b200..200cm_1950..2017_v0.2.tif;385MiB
# predicted250m;sol_texture.class_usda.tt_m_250m_b30..30cm_1950..2017_v0.2.tif;361MiB
# predicted250m;sol_texture.class_usda.tt_m_250m_b60..60cm_1950..2017_v0.2.tif;380MiB



# library(terra)
# 
# in.tif = "/vsicurl/https://s3.eu-central-1.wasabisys.com/openlandmap/predicted1km/pnv_fapar_proba.v.annual_d_1km_s0..0cm_2014..2017_v0.1.tif"
# 
# 
# tif = rast(in.tif)
# 
# xy = data.frame(lon=9.1126, lat=49.6466)
# terra::extract(tif, xy)
# 


## TODO:
# * comments
# * list caveats
# * sync values / texture ordering


library(raster)
library(rasterVis)
library(soilDB)
library(sf)
library(sp)
library(rgdal)
library(e1071)
library(aqp)
library(cluster)
library(png)
library(grid)


## AOI

# CA
.AOI_label <- 'CA'
.xmin <- -122.5
.xmax <- -119
.ymin <- 35
.ymax <- 39

# TX
.AOI_label <- 'TX'
.xmin <- -98
.xmax <- -94
.ymin <- 29
.ymax <- 33


# AR
.AOI_label <- 'AR'
.xmin <- -92
.xmax <- -88
.ymin <- 33
.ymax <- 37

# NE
.AOI_label <- 'NE'
.xmin <- -105
.xmax <- -98
.ymin <- 39
.ymax <- 43


# IN
.AOI_label <- 'IN'
.xmin <- -93
.xmax <- -84
.ymin <- 38
.ymax <- 42


# NM
.AOI_label <- 'NM'
.xmin <- -108
.xmax <- -101
.ymin <- 32
.ymax <- 37


# NC
.AOI_label <- 'NC'
.xmin <- -77.5
.xmax <- -76.5
.ymin <- 35.7
.ymax <- 36.2


# make a bounding box and assign a CRS (4326: GCS, WGS84)
AOI <- st_bbox(
  c(xmin = .xmin, xmax = .xmax, ymin = .ymin, ymax = .ymax), 
  crs = st_crs(4326)
)

# convert bbox to sf geometry
AOI <- st_as_sfc(AOI)

# texture triangle legend
txt.leg <- readPNG('E:/gis_data/FY2021-800m-rasters/soil-texture-legend-crop-small.png')
txt.leg <- as.raster(txt.leg)


# ISSR800 grid + RAT
texture_2550cm <- ISSR800.wcs(aoi = AOI, var = 'texture_2550cm')
txt.lut <- read.csv('http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_2550.csv')


# point grid for figures
s <- sampleRegular(texture_2550cm, size = 100, sp = TRUE)


## OLM

# create a compatible BBOX
e <- as(extent(texture_2550cm), 'SpatialPolygons')
proj4string(e) <- CRS('+init=EPSG:6350')
e.4326 <- spTransform(e, CRS('+init=EPSG:4326'))


# # local file
# x <- raster('E:/gis_data/landgis/sol_texture.class_usda.tt_m_250m_b30..30cm_1950..2017_v0.2.tif')

# COG access
x <- raster('/vsicurl/https://s3.eu-central-1.wasabisys.com/openlandmap/predicted250m/sol_texture.class_usda.tt_m_250m_b30..30cm_1950..2017_v0.2.tif')

# subset / warp to approximately ~250m resolution (800 / 3.2)
x <- crop(x, extent(e.4326))
x <- projectRaster(x, disaggregate(texture_2550cm, fact = 3.2, method = ''), method = 'ngb')

# init RAT
x <- ratify(x)

# extract integer codes from RAT
x.ll <- levels(x)[[1]]

# get official RAT: codes + labels 
rat <- read.csv('https://zenodo.org/record/2525817/files/sol_texture.class_usda.tt_m_250m_b_1950..2017_v0.1.tif.csv?download=1')

# manually re-code class abbreviations according to USDA defs
rat$class <- c('c', 'sic', 'sc', 'cl', 'sicl', 'scl', 'l', 'sil', 'sl', 'si', 'ls', 's', NA)

# merge codes + RAT
x.ll <- merge(x.ll, rat, by.x = 'ID', by.y = 'Value', all.x = TRUE)

# join colors used in ISSR8000 grids
x.ll$hex <- txt.lut$hex[match(x.ll$class, txt.lut$class)]

# re-arrange RAT + names
x.ll <- x.ll[, c('ID', 'class', 'hex', 'Name')]
names(x.ll) <- c('ID', 'class', 'hex', 'names')

# re-pack RAT
levels(x) <- x.ll

## harmonize grid codes
x.ll$newValue <- txt.lut$value[match(x.ll$class, txt.lut$class)]

# re-class
x.reclass <- reclassify(x, rcl = as.matrix(x.ll[, c('ID', 'newValue')]))


# make / edit / pack RAT
x.reclass <- ratify(x.reclass)
newRAT <- levels(x.reclass)[[1]]
newRAT <- merge(newRAT, x.ll[, c('newValue', 'class', 'hex', 'names')], by.x = 'ID', by.y = 'newValue')
levels(x.reclass) <- newRAT




## develop ISSR-800 figure

texture_2550cm.ll <- levels(texture_2550cm)[[1]]
pos <- 1:(nrow(texture_2550cm.ll) + 1)

sk <- list(
  space = 'bottom',
  at = pos,
  labels = list(
    labels = texture_2550cm.ll$class,
    at = pos - 0.5
  )
)


# use colors from RAT
cols <- levels(texture_2550cm)[[1]]$hex

# ISSR-800 map
p1 <- levelplot(
  texture_2550cm, 
  att = 'class', 
  maxpixels = 1e6, 
  main = 'ISSR-800 25-50cm (800m resolution)', 
  margin = FALSE, 
  col.regions = cols, 
  colorkey = sk, 
  scales = list(draw = FALSE)
) + layer(sp.points(s, pch = 3, col = 'black'))




# OLM map

cols2 <- newRAT$hex

# OLM soil texture
p2 <- levelplot(
  x.reclass, 
  att = 'class', 
  maxpixels = 1e6, 
  main = 'Open Land Map 30cm (250m resolution)', 
  margin = FALSE, 
  col.regions = cols2, 
  colorkey = list(space = 'bottom'), 
  scales = list(draw = FALSE)
) + layer(sp.points(s, pch = 3, col = 'black'))



## PNG output
fname <- sprintf('USDA-soil-texture-eval-%s.png', .AOI_label)
ragg::agg_png(file = fname, width = 2000, height = 1000, scaling = 1.25)

print(p1, more = TRUE, split = c(1, 1, 3, 1))
print(p2, more = FALSE, split = c(2, 1, 3, 1))

grid.raster(txt.leg, x = 0.95, y = 0.5, just="right", width = 0.25)

dev.off()




##
## lab data for AOI
##

# TODO: will time-out if too large
lab <- fetchKSSL(bbox = c(.xmax, .ymin, .xmin, .ymax))

## this is a rough approximation
# 30cm slice
lab.30 <- slice(lab, fm = 30 ~ sand + silt + clay + lab_texture_class, strict = FALSE)

# just in case, filter on non-NA coordinates
# this should have been done by fetchKSSL, 2nd check doesn't hurt
# ~ 1061 pedons
lab.30 <- subset(lab.30, !(is.na(x) | is.na(y)))

# init coords + CRS
coordinates(lab.30) <- ~ x + y
proj4string(lab.30) <- '+proj=longlat +datum=WGS84'

# cleanup
lab.30$lab_texture_class <- tolower(lab.30$lab_texture_class)
table(lab.30$lab_texture_class)

lab.30$texture <- ssc_to_texcl(sand = lab.30$sand, clay = lab.30$clay)
table(lab.30$texture)

# promote texture classes to site
site(lab.30) <- ~ texture + lab_texture_class


## TODO: correlate missing classes: e.g. lfs -> ls
# convert to factors
# missing classes will be converted to NA
lab.30$texture <- factor(lab.30$texture, levels = txt.lut$class)
lab.30$lab_texture_class <- factor(lab.30$lab_texture_class, levels = txt.lut$class)

# cross-check ~ 97% agreement
tab <- table(lab = lab.30$lab_texture_class, converted = lab.30$texture)
classAgreement(tab)


# SPC -> SPDF
lab.30.spdf <- as(lab.30, 'SpatialPointsDataFrame')

# transform to CRS of grids
lab.30.spdf <- spTransform(lab.30.spdf, CRS(projection(texture_2550cm)))

# keep just those points with texture
idx <- which(!is.na(lab.30.spdf$texture))
lab.30.spdf <- lab.30.spdf[idx, ]




# another with KSSL points
p1 <- levelplot(
  texture_2550cm, 
  att = 'class', 
  maxpixels = 1e6, 
  main = 'ISSR-800 25-50cm', 
  margin = FALSE, 
  col.regions = cols, 
  colorkey = sk, 
  scales = list(draw = FALSE)
) + layer(sp.points(lab.30.spdf, pch = 16, col = 'black', cex = 0.5))


p2 <- levelplot(
  x.reclass, 
  att = 'class', 
  maxpixels = 1e6, 
  main = 'Open Land Map 30cm (800m)', 
  margin = FALSE, 
  col.regions = cols2, 
  colorkey = list(space = 'bottom'), 
  scales = list(draw = FALSE)
) + layer(sp.points(lab.30.spdf, pch = 16, col = 'black', cex = 0.5))


fname <- sprintf('USDA-soil-texture-eval-%s-kssl.png', .AOI_label)
ragg::agg_png(file = fname, width = 2000, height = 1500, scaling = 1.5)

print(p1, more = TRUE, split = c(1, 1, 2, 1))
print(p2, more = FALSE, split = c(2, 1, 2, 1))

dev.off()



##
## quick cross-check between maps
##

ss <- sampleRegular(texture_2550cm, size = 5000, sp = TRUE)

a <- txt.lut$class[ss$Soil.Texture.Class..25.50cm]

b <- txt.lut$class[extract(x.reclass, ss)]

a <- factor(a, levels = txt.lut$class)
b <- factor(b, levels = txt.lut$class)

tab <- table(soilgrids = b, ISSR = a)

## TODO: create misclass costs from distance matrix
data("ROSETTA.centroids")

# convert name -> abbreviated code
idx <- match(ROSETTA.centroids$texture, SoilTextureLevels(which = 'names'))
ROSETTA.centroids$code <- SoilTextureLevels(which = 'codes')[idx]


m <- ROSETTA.centroids[, c('sat', 'fc', 'pwp')]
row.names(m) <- ROSETTA.centroids$code
d <- daisy(m)

# naieve conversion to similarity
sim <- (1 - as.matrix(d))
P <- apply(tab, 2, sum)/sum(tab)

map.concordance <- data.frame(
  classAgreement(tab, match.names = TRUE)[1:2],
  tauW(tab, W = sim, P = P)[c('tau', 'tau.w')]
)


# area proportions
fv1 <- factorValues(texture_2550cm, v = values(texture_2550cm), att = 'class')$class
fv2 <- factorValues(x.reclass, v = values(x.reclass), att = 'class')$class

fv1 <- factor(fv1, levels = txt.lut$class)
fv2 <- factor(fv2, levels = txt.lut$class)

t1 <- table(fv1)
t2 <- table(fv2)
t3 <- table(lab.30.spdf$texture)

tt <- cbind(
  KSSL = prop.table(t3),
  `ISSR-800` = prop.table(t1),
  OLM = prop.table(t2)
)


fname <- sprintf('USDA-soil-texture-eval-%s-area-prop.png', .AOI_label)
ragg::agg_png(file = fname, width = 1000, height = 500, scaling = 1.5)

par(mar = c(4.5, 5, 2.5, 1))
barplot(tt, las = 1, col = txt.lut$hex, horiz = TRUE, xlab = 'Proportion', legend.text = TRUE, args.legend = list(bty = 'n', x = 'top', horiz = TRUE, inset = -0.1), cex.axis = 0.85, cex.names = 0.85)

dev.off()

##
## Interesting note: point-wise comparisons are far better than I expected, 
## but illustrate an important point. point-wise PCC, K, etc. aren't sufficient evaluation
## when spatial patterns don't match
## 
##


##
## extract at lab data: result are factor codes
##

# ISSR-800
lab.30.spdf$ISSR800.texture <- txt.lut$class[extract(texture_2550cm, lab.30.spdf)]
lab.30.spdf$ISSR800.texture <- factor(lab.30.spdf$ISSR800.texture, levels = txt.lut$class)

# OLM
lab.30.spdf$OLM.texture <- txt.lut$class[extract(x.reclass, lab.30.spdf)]
lab.30.spdf$OLM.texture <- factor(lab.30.spdf$OLM.texture, levels = txt.lut$class)

# most frequent class in KSSL
most.frequent <- names(sort(table(lab.30.spdf$texture), decreasing = TRUE)[1])
lab.30.spdf$spike <- factor(most.frequent, levels = txt.lut$class)

# ~ 917 observations of soil texture
length(which(!is.na(lab.30.spdf$texture)))


tab <- table(spike = lab.30.spdf$spike, KSSL = lab.30.spdf$texture)
P <- apply(tab, 2, sum)/sum(tab)
tw <- tauW(tab, W = sim, P = P)
perf.spike <- data.frame(
  classAgreement(tab, match.names = TRUE)[1:2],
  tw[c('tau', 'tau.w')]
)

tab <- table(OLM = lab.30.spdf$OLM.texture, KSSL = lab.30.spdf$texture)
P <- apply(tab, 2, sum)/sum(tab)
tw <- tauW(tab, W = sim, P = P)
perf.OLM <- data.frame(
  classAgreement(tab, match.names = TRUE)[1:2],
  tw[c('tau', 'tau.w')]
)

tab <- table(ISSR800 = lab.30.spdf$ISSR800.texture, KSSL = lab.30.spdf$texture)
P <- apply(tab, 2, sum)/sum(tab)
tw <- tauW(tab, W = sim, P = P)
perf.ISSR800 <- data.frame(
  classAgreement(tab, match.names = TRUE)[1:2],
  tw[c('tau', 'tau.w')]
)


perf <- make.groups(
  KSSL.spike = perf.spike, 
  KSSL.vs.OLM = perf.OLM, 
  KSSL.vs.ISSR800 = perf.ISSR800, 
  map.concordance
)

knitr::kable(perf[, 1:4], digits = 2)





