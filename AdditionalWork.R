# Energy Consumption Differences

# maximum difference in energy consumption
# From calculations 15 Oct 2015

# Based on detailed timeline
# UNITS ARE JOULES
# min_Wa and max_Wa from PowerIncrease.m
diff_min_detailed <- c(231120000,27113184000.0000,11104128000.0000,1772928000.00000,442368000,
                       4250016000.00000,7634304000.00000,976320000,30405888000.0000,13969584000.0000,
                       3978720000.00000,6069600000.00000,1763424000.00000,38754720000.0000,13106880000.0000)
diff_max_detailed<- c(10631520000.0000,29206656000.0000,533246112000.000,22664448000.0000,
                      2540160000.00000,19309536000.0000,10924416000.0000,59597856000.0000,
                      31478976000.0000,173087280000.000,14105664000.0000,20187360000.0000,
                      41961888000.0000,209667744000.000,395537472000.000)

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
# text(1,1E13,'A',cex = 1.2)
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
