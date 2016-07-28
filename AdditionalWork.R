# Energy Consumption Differences

# maximum difference in energy consumption
# From calculations 15 Oct 2015

# Based on detailed timeline
# UNITS ARE JOULES
# min_Wa and max_Wa from PowerIncrease.m
diff_min_detailed <- c(128446560,8610719040,5167503234.9216,760075527.9168,175823096.131584,
                       1680180497.9424,3158801652.21120,387940652.294400,8568290978.13888,
                       2250115200.00000,2362608000.00000,1040256000.00000,749952000,
                       17978976000.000,6277364697.42720)
diff_max_detailed<- c(5908541760,9007828992,246109406289.84,4159838432.8512,385815052.812096,
                      7473085163.3952,4457352499.26912,10426641168.2573,8764673499.216,
                      31413398400,8346585600,2561656320,16498080000,96063840000,
                      161035266054.931)

fate <- c(0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1) #  0 = alive, 1 = dead

# load required libraries
require(vioplot)
require(devtools)
require(digest)
require(beanplot)

# set directory for pdf
setwd("~/Documents/R/TOWR/AnalysisFigures") # set directory to figures

pdf("WorkDiff_beanplot_detailed.pdf",width = 5,height = 5)
op <- par
op <- par(mfrow = c(1,2),
          oma = c(0,0,0,0) + 0.1,
          mar = c(2,4,0,0) + 0.1)

beanplot(diff_min_detailed ~ fate,ll = 0.25, log="y", method = "stack", bw = "nrd0",
         side = "both" , ylab="Additional Work (J)",axes = FALSE, ylim = c(1E7,1E13),
         col = list("white", c("red", "black")), beanlinewd = 1.75)
lines(c(-1,3),c(8.57E9,8.57E9),col = "black",lty=2)
axis(2,at=c(1E7,1E8,1E9,1E10,1E11,1E12),labels = c(expression(paste("10"^"7")),
                                              expression(paste("10"^"8")),
                                              expression(paste("10"^"9")),
                                              expression(paste("10"^"10")),
                                              expression(paste("10"^"11")),
                                              expression(paste("10"^"12"))))

beanplot(diff_max_detailed ~ fate,ll = 0.25, log="y",names = c("",""), bw = "nrd0",
         side = "both", axes = FALSE, ylim = c(1E7,1E13),method = "stack",
         col = list("white", c("red", "black")), beanlinewd = 1.75)
axis(2,at=c(1E8,1E9,1E10,1E11,1E12,1E13),labels = c(expression(paste("10"^"8")),
                                              expression(paste("10"^"9")),
                                              expression(paste("10"^"10")),
                                              expression(paste("10"^"11")),
                                              expression(paste("10"^"12")),
                                              expression(paste("10"^"13"))))
# text(1,1E13,'B',cex = 1.2)
dev.off()

# DO WITH COLOURS FOR LIVE AND DEAD
pdf("WorkDiff_beanplot_detailed2.pdf",width = 5,height = 5)
op <- par
op <- par(mfrow = c(1,2),
          oma = c(0,0,0,0) + 0.1,
          mar = c(2,4,0,0) + 0.1)

beanplot(diff_min_detailed[fate==0],ll = 1, col = c(0,"steelblue","steelblue","steelblue"), what=c(1,0,1,1),
         side = "both",method = "stack", add=FALSE, log = "y",beanlinewd = 2,
         ylab="Additional Work (J)",axes = FALSE, ylim = c(1E7,1E13))
beanplot(diff_min_detailed[fate==1],ll = 1, col = c(0,"firebrick","firebrick","firebrick"), what=c(1,0,1,1),
         side = "both",method = "stack", add=TRUE, log = "y",beanlinewd = 2,
         ylab="Additional Work (J)",axes = FALSE, ylim = c(1E7,1E13))
beanplot(diff_min_detailed,ll = 1, col = c("#00000000","#FFFFFFFF","#FFFFFFFF","#FFFFFFFF"), what=c(0,1,0,0),
         side = "both", method = "stack", add=TRUE, log = "y",
         ylab="Additional Work (J)",axes = FALSE, ylim = c(1E7,1E13))
# add axis information
axis(2,at=c(1E7,1E8,1E9,1E10,1E11,1E12),labels = c(expression(paste("10"^"7")),
                                                   expression(paste("10"^"8")),
                                                   expression(paste("10"^"9")),
                                                   expression(paste("10"^"10")),
                                                   expression(paste("10"^"11")),
                                                   expression(paste("10"^"12"))))

beanplot(diff_max_detailed[fate==0],ll = 1, col = c(0,"steelblue","steelblue","steelblue"), what=c(1,0,1,1),
         side = "both", method = "stack", add=FALSE, log = "y",beanlinewd = 2,
         axes = FALSE, ylim = c(1E7,1E13))
beanplot(diff_max_detailed[fate==1],ll = 1, col = c(0,"firebrick","firebrick","firebrick"), what=c(1,0,1,1),
         side = "both", method = "stack", add=TRUE, log = "y",beanlinewd = 2,
         axes = FALSE, ylim = c(1E7,1E13))
beanplot(diff_max_detailed,ll = 1, col = c("#00000000","#FFFFFFFF","#FFFFFFFF","#FFFFFFFF"), what=c(0,1,0,0),
         side = "both", method = "stack", add=TRUE, log = "y",axes = FALSE, ylim = c(1E7,1E13))
# add axis information
axis(2,at=c(1E8,1E9,1E10,1E11,1E12,1E13),labels = c(expression(paste("10"^"8")),
                                                   expression(paste("10"^"9")),
                                                   expression(paste("10"^"10")),
                                                   expression(paste("10"^"11")),
                                                   expression(paste("10"^"12")),
                                                   expression(paste("10"^"13"))))

     dev.off()
