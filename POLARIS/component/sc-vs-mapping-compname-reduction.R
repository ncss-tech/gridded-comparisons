library(plyr)

m <- read.table('mapping.txt', stringsAsFactors=FALSE)
s <- read.csv('compname-stats.csv.gz', stringsAsFactors=FALSE)

names(m) <- c('id', 'compname')
names(s) <- c('compname', 'ac')

m$compname <- toupper(m$compname)


g <- join(s, m, by='compname', type='left')
