## Plot effect of depth

## Run Depth AIC Code 
setwd("~/Documents/R/TOWR/")
source("DepthEffect_AIC.R")

# REORDER DPTH TO BE THE SAME AS IN CLUSTER DIAGRAM
dpth <- c("20120912_J011409.csv","20120912_J120305.csv","20120912_telembuoy.csv",
          "20120912_8mm200m.csv","20120912_J072498.csv","20120912_8mm100m.csv",
          "20120912_J070602.csv","20120912_J020709.csv")

## Create the data
## the matrix containing data for Figure 02a
H.mat <- matrix(NA, nrow=8*3, ncol=3)

# loop through files
for(i in 1:length(dpth)){
  
# load data
dat <- read.csv(dpth[i],header=FALSE)
# sort by speed
sortdat <- dat[order(dat$V2),]

H.mat[i*3-2, 1:3] = t(sortdat$V3[1:3])
H.mat[i*3-1, 1:3] = t(sortdat$V3[4:6])
H.mat[i*3, 1:3] = t(sortdat$V3[7:9])

}

library(reshape2)

## Add row and column names
dpth_short <- sub(".*?20120912_(.*?).c.*", "\\1", dpth)
dpth_rep <-rep(dpth_short,each=3)
dpth_all <- vector(mode="character",length=24)
dpth_all[seq(0,24,3)] = dpth_rep[seq(0,24,3)]

rownames(H.mat) <- dpth_all
colnames(H.mat) <- c(0,3,6)
names(dimnames(H.mat)) <- c('Set','Depth')
H.df<-melt(H.mat)

## Create colour palette
YlOrBr <- c("#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB",
            "#FFFFD9","#41B6C3", "#1D91C0", "#225ea8", "#253494", "#081D58","#000000")
my_palette <- colorRampPalette(YlOrBr, space = "Lab")(n = 1000)



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
          dendrogram = "none", Colv = "FALSE", Rowv = "FALSE", 
          # don't add dendrograms, don't sort by row or column
          col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256), 
          scale = "row", # scale by row
          rowsep=seq(0, 24, 3), # separate rows by gear set
          cexRow = 1, srtCol = 0, # rotate column labels
          labCol = c("0 m","3 m", "6 m"), adjCol = c(0.5,1),
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,7)),
          symkey = FALSE, symm = FALSE, # don't need symmetry in key (postive values only)
          xlab = "DepthEffect_Heatmap_scale.pdf")
dev.off()


## Plot with no row scaling
pdf("DepthEffect_Heatmap_NOscale.pdf", width = 10, height = 8)
par(mar=c(0,0,0,8))
par(oma=c(0,0,0,8))
heatmap.2(H.mat, cellnote = H.mat, density.info = "none", trace = "none", 
          dendrogram = "none", Colv = "FALSE", Rowv = "FALSE", 
          # don't add dendrograms, don't sort by row or column
          col = my_palette, 
          rowsep=seq(0, 24, 3), # separate rows by gear set
          cexRow = 1, srtCol = 0, # rotate column labels
          labCol = c("0 m","3 m", "6 m"), adjCol = c(0.5,1),
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,7)),
          symkey = FALSE, symm = FALSE, # don't need symmetry in key (postive values only)
          xlab = "DepthEffect_Heatmap_NOscale.pdf") # change margins
dev.off()

## Plot with row scaling, and with sorting by rows
rownames(H.mat) <- dpth_rep
pdf("DepthEffect_Heatmap_scale_SORTED.pdf",width = 10,height = 8)
heatmap.2(H.mat, cellnote = H.mat, density.info = "none", trace = "none",
            Colv = "FALSE", labRow = dpth_rep,
            # don't add dendrograms, don't sort by row or column
            col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256),
            scale = "row", # scale by row
            rowsep = c(4,5,12,16,20,23), # separate rows by gear set
            cexRow = 1, srtCol = 0, # rotate column labels
            labCol = c("0 m","3 m", "6 m"), adjCol = c(0.5,1),
            lmat = lmat, key.par=list(mar=c(3.5,0,9.5,7)),
            symkey = FALSE, symm = FALSE, # don't need symmetry in key (postive values only)
)
dev.off()

