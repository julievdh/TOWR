## Create the data
# palette <- brewer.pal(12,"Paired")
palette <- colorRampPalette(c("blue","red"))(n = 24*3)

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

library(ggplot2)
library(reshape2)
#rownames(H.mat) <- files[delAIC< -2]
colnames(H.mat) <- c(0,3,6)
names(dimnames(H.mat)) <- c('Set','Depth')
H.df<-melt(H.mat)

# for continuous values
col <- palette[match(ordered(H.df$value),levels(ordered(H.df$value)))]

ggplot(H.df,aes(x=Depth,y=Set,fill=col)) + 
  geom_tile(colour='black') + scale_x_continuous(breaks=c(0,3,6))


# ggplot(H.df,aes(x=Depth,y=Set,fill=value)) + geom_tile(colour='black') + scale_x_continuous(breaks=c(0,3,6))

#######
my_palette <- colorRampPalette(c("white","red", "blue"))(n = 299)

heatmap.2(H.mat, density.info = "none", trace = "none", Colv = "NA",Rowv = "FALSE",col = my_palette, scale = "row")

#######
