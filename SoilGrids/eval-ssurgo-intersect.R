
library(e1071)
library(plyr)

# intersect with SSURGO, dominant suborder / map unit
x <- read.csv('soilgrids_samples.csv', stringsAsFactors=FALSE)

# mapped factor levels are the gold standard
l.mapped <- unique(x$taxsuborder)
l.predicted <- unique(x$predicted_suborder)

# cross-tabulate actual vs. predicted
tab <- table(x$taxsuborder, x$predicted_suborder)

# eval class-agreement
round(unlist(classAgreement(tab, match.names = TRUE)), 3)

# sub orders mapped but not predicted
mnp <- l.mapped[which(! l.mapped %in% l.predicted)]

# sub orders predicted but not mapped
pnm <- l.predicted[which(! l.predicted %in% l.mapped)]
