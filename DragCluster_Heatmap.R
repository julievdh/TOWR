## set directory
setwd("~/Documents/R/TOWR")

## find all filenames
files <- list.files(path = "./TOW_data", pattern = ".csv")

## the matrix containing data for Files affected by depth
H.mat <- matrix(NA, nrow=length(files)*3, ncol=3)

# loop through files
for(i in 1:length(files)){
  
  # load data
  setwd("~/Documents/R/TOWR/TOW_data")
  dat <- read.csv(files[i],header=FALSE)
  
  H.mat[i*3-2, 1:3] = t(dat$V3[1:3]) 
  H.mat[i*3-1, 1:3] = t(dat$V3[4:6])
  H.mat[i*3, 1:3] = t(dat$V3[7:9])
}

## plot clustered 
## each row is a different depth, columns represent speeds
library(gplots)
library(RColorBrewer)
YlOrBr <- c("#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB","#FFF7FB",
  "#FFFFD9","#41B6C3", "#1D91C0", "#225ea8", "#253494", "#081D58","#000000")
my_palette <- colorRampPalette(YlOrBr, space = "Lab")(n = 1000)
heatmap.2(H.mat, density.info = "none", trace = "none", col = my_palette, symkey = FALSE)


## matrix containing data for all depths/tows/speeds
H.mat2 <- matrix(NA, nrow=length(files), ncol=9)

# loop through files
for(i in 1:length(files)){
  
  # load data
  dat <- read.csv(files[i],header=FALSE)
  # sort by speed
  # sortdat <- dat[order(dat$V2),]
  
  H.mat2[i, 1:9] = t(dat$V3)

}

# attach file names for rows
rownames(H.mat2) <- sub(".*?20120912_(.*?).c.*", "\\1", files)

row_distance = dist(H.mat2, method = "manhattan")
row_cluster = hclust(row_distance, method = "ward.D")
col_distance = dist(t(H.mat2), method = "manhattan")
col_cluster = hclust(col_distance, method = "ward.D")

heatmap.2(H.mat2, density.info = "none",   
          Rowv = as.dendrogram(row_cluster), Colv = as.dendrogram(col_cluster),
          trace = "none", col = redgreen(75), 
          scale = "row", cexRow = 1, cexCol = 1, 
          symkey = FALSE,symm = FALSE, 
          srtCol = 0, # rotate column labels
          margins = c(5,13))

# ORDER BY DEPTH
depth_ordered <- H.mat2[,c(1,4,7,2,5,8,3,6,9)]
row_distance = dist(depth_ordered, method = "manhattan")
row_cluster = hclust(row_distance, method = "ward.D")

setwd("~/Documents/R/TOWR/AnalysisFigures") # set directory to figures

# matrix for positions in heat map 
# (1 = heatmap, 2 = row dendrogram, 3 = column dendrogram, 4 = key)
lmat = rbind(c(3,4),c(2,1)) 

par(mar=c(0,0,0,9))
par(oma=c(0,0,0,9))
pdf("Drag_Heatmap_scale.pdf",width = 10,height = 8)
heatmap.2(depth_ordered, density.info = "none",   
          dendrogram = "row", Rowv = as.dendrogram(row_cluster), 
          Colv = "NA", trace = "none", 
          scale = "row", col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256), 
          cexRow = 1, cexCol = 2, 
          srtCol = 0, # rotate column labels
          rowsep = c(3,8,9,15), sepwidth = c(0.1,0.1), # add row separation for major clusters
          symkey = FALSE,symm = FALSE, 
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,11)),
          margins = c(3,10))
dev.off()

pdf("Drag_Heatmap_NOscale.pdf",width = 10,height = 8)
heatmap.2(depth_ordered, density.info = "none",   
          dendrogram = "row", Rowv = as.dendrogram(row_cluster), 
          Colv = "NA", trace = "none", 
          col = my_palette, 
          cexRow = 1, cexCol = 2, 
          symkey = FALSE,symm = FALSE,
          rowsep=c(3,8,9,15), sepwidth = c(0.1, 0.1), # add row separation for major clusters
          srtCol = 0, # rotate column labels
          lmat = lmat, key.par=list(mar=c(3.5,0,9.5,11)),
          margins = c(3,10))
dev.off()
