library(latticeExtra)
library(hexbin)


x <- read.csv('points-for-ssurgo-query/dssurgo_samples.csv.gz', stringsAsFactors=FALSE)

xyplot(actual_comppct ~ prob, data=x, asp=1, xlab='Probability of 1st, 2nd, and 3rd Predicted Classes', ylab='Mapped Component Percent (SSURGO)', scales=list(cex=1.25), col=rgb(0,0,0, alpha=0.125)) + layer(panel.grid(-1, -1, col='grey')) + layer(panel.abline(a=0, b=1, col='red', lwd=2))

pdf(file='prob_vs_ssurgo.pdf', height=10, width=10)
hexbinplot(actual_comppct ~ prob, data=x, asp=1, xlab='Probability of 1st, 2nd, and 3rd Predicted Classes', ylab='Mapped Component Percent (SSURGO)', scales=list(cex=1.25)) + layer(panel.grid(-1, -1, col='white')) + layer(panel.abline(a=0, b=1, col='red', lwd=2))
dev.off()

# correlation
cor(x$prob, x$actual_comppct, method='spearman', use='pairwise.complete.obs')
cor(x$prob, x$actual_comppct, method='pearson', use='pairwise.complete.obs')
