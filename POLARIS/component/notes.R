library(raster)
library(e1071)
library(caret)

# NOTE: there are errors in the quoting (manually fixed)
# lookup table
lu <- read.table('mapping.txt', header=FALSE)

## had to manuall convert to TIFF, georef information is missing

# the 1st (?) most likely class?
x <- raster('lat3940_lon-121-120-0.tif')

# the 1st (?) most like class probability
p <- raster('lat3637_lon-120-119-1.tif')

# this is the orignal (?) gSSURGO data
# most common component (?)
y <- raster('lat3940_lon-121-120-100.tif')

# set the nodata value (-9999)
NAvalue(x) <- -9999
NAvalue(y) <- -9999

# stack for sampling
s <- stack(x, y)
names(s) <- c('ml_class_1', 'original')

# check ranges
summary(s)
hist(s, breaks=100)

# sample both
ss <- sampleRegular(s, size=10000)

# which components were predicted, but not mapped
pred.not.mapped <- unique(ss[, 1])[which(! unique(ss[, 1]) %in% unique(ss[, 2]))]

# which components were mapped, but not predicted
mapped.not.pred <- unique(ss[, 2])[which(! unique(ss[, 2]) %in% unique(ss[, 1]))]

lu[lu$V1 %in% pred.not.mapped, ]
lu[lu$V1 %in% mapped.not.pred, ]



## this isn't correct because it assumes that the levels of mapped / predicted are the same
# classAgreement(table(ss[, 1], ss[, 2]),  match.names=FALSE)

## this isn't correct because it only looks at the classes common to both mapped / predicted
# classAgreement(table(ss[, 1], ss[, 2]),  match.names=TRUE)

## this is correct
# levels of "mapped" are the gold standard, anything outside of these values isn't helpful
mapped <- factor(ss[, 2])
# force levels to those of "mapped", all other values set to NA
predicted <- factor(ss[, 1], levels=levels(mapped))

cm <- confusionMatrix(data=predicted, reference=mapped)
cm$overall

# this is now correct
classAgreement(table(mapped, predicted))



# save comparison
png(file='comp-1.png', width=2100, height=1000, res=120)
plot(s, maxpixels=1000000)
dev.off()

# eval probability distribution for most likely class
png(file='comp-1-pr.png', width=2100, height=1000, res=120)
par(mfcol=c(1,2))
plot(p, maxpixels=1000000, main='Pr(most likely class')
hist(p, breaks=100, main='Pr(most likely class')
dev.off()



## come up with some places to eval the product
library(maps)
library(maptools)

# get conus map and sample it
m <- map('county', 'ca')
m.ids <- 1:length(maptools:::.NAmat2xyList(cbind(m$x, m$y)))
usa <- map2SpatialPolygons(m, IDs=m.ids)
proj4string(usa) <- '+proj=longlat +datum=NAD83'
s <- spsample(usa, n = 40, type='regular')

plot(usa)
points(s)

# truncate coordinates, and get unique set
coords <- unique(round(coordinates(s)))

# convert into file names
f <- paste0('lat', coords[, 2], coords[, 2] + 1, '_lon', coords[, 1], coords[, 1] + 1, '.nc')

# dump in text format
cat(f, sep = '\n')




