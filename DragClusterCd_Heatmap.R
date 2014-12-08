## load libraries
library(gplots)
library(RColorBrewer)
library(R.matlab)

## set directory
setwd("~/Documents/MATLAB/TOW/")

## load data
## each row is a different gear set
## columns are 0 m speed 1; 0 m speed 2; 0 m speed 3; 3 m speed 1; etc.
df <- readMat('CdRe_data.mat')

## set directory back
setwd("~/Documents/R/TOWR")

YlOrBr <- c("#FFF7FB","#41B6C3", "#1D91C0", "#225ea8", "#253494","#081D58",
 "#081D58","#000000","#000000","#000000")
my_palette <- colorRampPalette(YlOrBr, space = "Lab")(n = 1000)

# attach file names for rows
rownames(df$Cd) <- sub(".*?20120912_","",df$name)
colnames(df$Cd) <- c("d1s1","d1s2","d1s3","d2s1","d2s2","d2s3","d3s1","d3s3","d3s3")
rowcat = c(0,1,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,1,1,0)
rowcol = c("#B30000",rep("#FECC5C",4),"#B30000",rep("#FECC5C",3),"#B30000","#B30000","#FECC5C","#FECC5C",
           "#B30000",rep("#FECC5C",6),"#B30000")

# matrix for panel positions in heat map 
# (1 = heatmap, 2 = row dendrogram, 3 = column dendrogram, 4 = key)
lmat = rbind(c(3,4),c(2,1)) 

# calculate dendrogram
row_distance = dist(df$Cd, method = "manhattan")
row_cluster = hclust(row_distance, method = "ward.D")
col_distance = dist(t(df$Cd), method = "manhattan")
col_cluster = hclust(col_distance, method = "ward.D")

setwd("~/Documents/R/TOWR/AnalysisFigures") # set directory to figures

par(mar=c(0,0,0,9))
par(oma=c(0,0,0,9))
pdf("Cd_Heatmap_scale_bydepth.pdf",width = 10,height = 8)
heatmap.2(df$Cd, density.info = "none",
          dendrogram = "row", Rowv = as.dendrogram(row_cluster),
          Colv = "NA", trace = "none",
          scale = "row", col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256),
          cexRow = 1, cexCol = 2,
          srtCol = 45, # rotate column labels
          rowsep=c(1,2,3,7), sepwidth = c(0.1,0.1), # add row separation for major clusters
          symkey = FALSE,symm = FALSE,
          lmat = lmat, key.par=list(mar=c(3.5,0,6,11)),
          lhei = c(0.25,0.95), margins = c(4,10))
dev.off()

pdf("Cd_Heatmap_NOscale_bydepth.pdf",width = 10,height = 8)
heatmap.2(df$Cd, density.info = "none",
          dendrogram = "row", Rowv = as.dendrogram(row_cluster),
          Colv = "NA", trace = "none",
          col = my_palette,
          cexRow = 1, cexCol = 2,
          srtCol = 45, # rotate column labels
          rowsep=c(1,2,3,7), sepwidth = c(0.1,0.1), # add row separation for major clusters
          symkey = FALSE,symm = FALSE,
          lmat = lmat, key.par=list(mar=c(3.5,0,6,11)),
          lhei = c(0.25,0.95), margins = c(4,10))
dev.off()

# ORDER BY SPEED
speed_ordered <- df$Cd[,c(1,4,7,2,5,8,3,6,9)]
row_distance = dist(speed_ordered, method = "manhattan")
row_cluster = hclust(row_distance, method = "ward.D")

par(mar=c(0,0,0,9))
par(oma=c(0,0,0,9))
pdf("Cd_Heatmap_scale.pdf",width = 10,height = 8)
heatmap.2(speed_ordered, density.info = "none",   
          dendrogram = "row", Rowv = as.dendrogram(row_cluster), 
          Colv = "NA", trace = "none", 
          RowSideCol = rowcol,
          scale = "row", col = colorRampPalette(brewer.pal(9, "YlGnBu"))(256), 
          cexRow = 1, cexCol = 2, 
          srtCol = 45, # rotate column labels
          rowsep=c(1,2,3,7), sepwidth = c(0.1,0.1), # add row separation for major clusters
          symkey = FALSE,symm = FALSE, 
          key.par=list(mar=c(3.5,0,6,11)),
          lhei = c(0.25,0.95), margins = c(4,8))
dev.off()

pdf("Cd_Heatmap_NOscale.pdf",width = 10,height = 8)
heatmap.2(speed_ordered, density.info = "none",   
          dendrogram = "row", Rowv = as.dendrogram(row_cluster), 
          Colv = "NA", trace = "none", 
          RowSideCol = rowcol,
          col = my_palette,
          cexRow = 1, cexCol = 2, 
          symkey = FALSE,symm = FALSE,
          rowsep=c(1,2,3,7), sepwidth = c(0.1, 0.1), # add row separation for major clusters
          srtCol = 45, # rotate column labels
          key.par=list(mar=c(3.5,0,6,11)),
          lhei = c(0.25,0.95), margins = c(4,10))
dev.off()

# # What are the counts in different cluster numbers?
# counts = sapply(2:7,function(ncl)table(cutree(row_cluster,ncl)))
# # Plot with specified number of clusters
# op <- par(mar = c(1,4,4,1), mfrow = c(1,1))
# plot(row_cluster, cex = 0.6, main = "Ward showing 5 clusters")
# rect.hclust(row_cluster, k = 5)
# 
# # compute hierarchical clustering using different linkage types
# pr.hc.s <- hclust(row_distance, method = 'single')
# pr.hc.c <- hclust(row_distance, method = 'complete')
# pr.hc.a <- hclust(row_distance, method = 'average')
# pr.hc.w <- hclust(row_distance, method = 'ward.D')
# 
# # plot them
# frame()
# op <- par(mar = c(0,4,4,2), mfrow = c(2,2))
# 
# plot(pr.hc.s, labels = FALSE, main = "Single", xlab = "")
# rect.hclust(pr.hc.s, k = 5)
# plot(pr.hc.c, labels = FALSE, main = "Complete", xlab = "")
# rect.hclust(pr.hc.c, k = 5)
# plot(pr.hc.a, labels = FALSE, main = "Average", xlab = "")
# rect.hclust(pr.hc.a, k = 5)
# plot(pr.hc.w, labels = FALSE, main = "Ward", xlab = "")
# rect.hclust(pr.hc.w, k = 5)


# Do K means clustering and hierarchical with 5 and see whether clusters are the same
# set 5 clusters
clusters.w = cutree(hclust(row_distance, method = 'ward.D'), k=5) 

Cd.km <- kmeans(df$Cd, centers = k, nstart = 100)
clust.centers <- Cd.km$centers





pdf("Cd_Clusters.pdf",width = 10,height = 8)
#Look at all clusters
op <- par(mfrow = c(2, 3))
for(clusterNum in 1:5) {
  
  # Set up the axes without plotting; ylim set based on trial run.
  plot(clust.centers[clusterNum,1:9], ylim = c(0,0.4),
       ylab = " ",
       axes = F, ) 
  axis(2)
  axis(1, 1:9, c(colnames(clust.centers)[1:9]), cex.axis = 0.9)
  
  # Plot the expression of all the genes in the selected cluster in grey.
  matlines(y = t(df$Cd[Cd.km$cluster == clusterNum, 1:9]),
           col = 'grey') 
  
  # Add the cluster center. This is last so it isn't underneath the members
  points(clust.centers[clusterNum, 1:9] , type = 'l') 
  
  # Optional: points to show development stages.
  points(clust.centers[clusterNum, 1:9],  pch = 20)
} 
dev.off()