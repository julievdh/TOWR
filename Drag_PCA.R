## Create large matrix for PCA 
## 10 Nov 2014

# set directory
setwd("~/Documents/R/TOWR")

# find all filenames
files <- list.files(path = "./TOW_data", pattern = ".csv")

# Case Information
sink = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,1,0,0)
float = c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1)
length = c(100,150,200,25,50,275,15,19,NA,10,37,82,NA,NA,NA,16,NA,
               122,143,243,19)
weight = c(NA,NA,NA,NA,NA,25.8,0.7,0.7,10.55,2.6,3.75,9.65,2.95,
               2.9,4.6,15.7,0.9,8.85,13.45,9.50,18.2)
lineonly = c(1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,0,1,0,0,1,0)
min = c(NA,NA,NA,NA,NA,51,32,6,163,100,57,422,68,335,NA,1,119,9,25,11,NA)
max = c(NA,NA,NA,NA,NA,392,206,39,768,1136,266,485,100,705,NA,24,433,296,96,249,NA)
dead = c(NA,NA,NA,NA,NA,5,5,5,1,1,1,5,5,1,5,1,5,5,5,5,5)
sex = c(NA,NA,NA,NA,NA,0,1,2,1,2,1,2,1,2,1,2,1,1,1,0,NA)

params <- rbind(float,sink,length,weight,lineonly)
colnames(params) <- sub(".*?20120912_(.*?).c.*", "\\1", files)

library(gplots)
heatmap.2(params,trace = "none", density.info = "none", 
          col = colorRampPalette(c("green","black","red")),
          scale = "row",symm = FALSE, symkey = FALSE)

# now make in to 9x each for use in PCA
sink = rep(sink, each = 9)
float = rep(float, each = 9)
length = rep(length, each = 9)
weight = rep(weight, each = 9)
lineonly = rep(lineonly, each = 9)
min = rep(min, each = 9)
max = rep(max, each = 9)
dead = rep(dead, each = 9)
sex = rep(sex,each = 9)


# load first file and convert to matrix
i = 1
setwd("~/Documents/R/TOWR/TOW_data")
dat <- read.csv(files[i],header=FALSE)
name = rep(sub(".*?20120912_(.*?).c.*", "\\1", files[i]), each = 9)

H.mat = cbind(name,dat)

# loop through files and combine with previous
for(i in 2:length(files)){
  
  # load data
  setwd("~/Documents/R/TOWR/TOW_data")
  dat <- matrix(NA, nrow = 9, ncol = 4)
  dat <- read.csv(files[i],header = FALSE)
  name = rep(sub(".*?20120912_(.*?).c.*", "\\1", files[i]), each = 9)

  # attach previous file with data and name of new data
H.mat = rbind(H.mat,cbind(name,dat))

}

# attach column names
colnames(H.mat) <- c("Name","Depth","Speed","Drag")

# combine drag matrix with parameters
alldat <- cbind(H.mat,weight,length,lineonly,sink,float,min,max,sex,dead)

## Do PCA while ignoring any NAs for length, weight, etc. 
# exclude NAs
compdata <- alldat[complete.cases(alldat[,1:9]),]

fit <- prcomp(compdata[,2:6], center = T, scale = T, rotation = "varimax") # SVD with varimax rotation
# summary(fit)
biplot(fit,xlabs = compdata$Name)

dotchart(sort(fit$rotation[, 1]), main = "First Principal Component")
dotchart(sort(fit$rotation[, 2]), main = "Second Principal Component")

ggplot(data.frame(cbind(fit$scores[, 1:2], compdata$Drag))) + geom_point(aes(fit$x[,1], fit$x[,2], 
                 colour = as.factor(compdata$lineonly)))
