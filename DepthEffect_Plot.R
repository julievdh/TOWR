## Plot effect of depth

## Run Depth AIC Code 
setwd("~/Documents/R/TOWR/")
source("DepthEffect_AIC.R")

## Create the data
## the matrix containing data for Figure 02a
H.mat <- matrix(NA, nrow=8*3, ncol=3)

# loop through files
for(i in 1:length(dpth)){
  
# load data
dat <- read.csv(dpth[i],header=FALSE)
# sort by speed
sortdat <- dat[order(dat$V2),]

H.mat[i*3-2, 1:3] = t(dat$V3[1:3])
H.mat[i*3-1, 1:3] = t(dat$V3[4:6])
H.mat[i*3, 1:3] = t(dat$V3[7:9])

}

library(reshape2)

## Add row and column names
rownames(H.mat) <- rep(dpth,each=3)
colnames(H.mat) <- c(0,3,6)
names(dimnames(H.mat)) <- c('Set','Depth')
H.df<-melt(H.mat)

## Create colour palette
my_palette <- colorRampPalette(c("white","red", "blue"))(n = 299)

## Plot heatmap
library(gplots)
library(RColorBrewer)

setwd("~/Documents/R/TOWR/AnalysisFigures") # set directory to figures

## Scale colors for each row
lmat = rbind(c(3,4),c(2,1))

par(mar=c(0,0,0,8))
par(oma=c(0,0,0,8))
pdf("DepthEffect_Heatmap_scale.pdf",width = 10,height = 8)
heatmap.2(H.mat, density.info = "none", trace = "none", 
          dendrogram = "none", Rowv = "FALSE", # don't add dendrograms, don't sort by row
          col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256), 
          scale = "row", # scale by row
          rowsep=seq(0, 24, 3), # separate rows by gear set
          cexRow = 0.8, srtCol = 0, # rotate column labels
          labCol = c("0 m","3 m", "6 m"), adjCol = c(0.5,1),
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,7)),
          symkey = FALSE, symm = FALSE, # don't need symmetry in key (postive values only)
          )
dev.off()


## Plot with no row scaling
pdf("DepthEffect_Heatmap_NOscale.pdf", width = 10, height = 8)
par(mar=c(0,0,0,8))
par(oma=c(0,0,0,8))
heatmap.2(H.mat, density.info = "none", trace = "none", 
          dendrogram = "none", Rowv = "FALSE", # don't add dendrograms, don't sort by row
          col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256), 
          rowsep=seq(0, 24, 3), # separate rows by gear set
          cexRow = 0.8, srtCol = 0, # rotate column labels
          labCol = c("0 m","3 m", "6 m"), adjCol = c(0.5,1),
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,7)),
          symkey = FALSE, symm = FALSE, # don't need symmetry in key (postive values only)
          ) # change margins
dev.off()

