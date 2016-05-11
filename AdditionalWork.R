# Energy Consumption Differences

# maximum difference in energy consumption
# From calculations 15 Oct 2015

# Based on detailed timeline
# UNITS ARE JOULES
# min_Wa and max_Wa from PowerIncrease.m
diff_min_detailed <- c(123586560,10795645440.0000,5314118400.00000,809326080,
                       193648320.000000,1850886720.00000,3408307200.00000,
                       443836800,11072332800.0000,3258489600.00000,2336688000.00000,
                       1795305600.00000,800064000,18498240000.0000,7654953600.00000)
diff_max_detailed<- c(5684981760.00000,11607891840.0000,253909097280.000,6898798080.00000,
                      747169920.000000,8301078720.00000,4837708800.00000,17827905600.0000,
                      11386128960.0000,44094240000.0000,8339846400.00000,5414083200.00000,
                      19946476800.0000,99313344000.0000,210661879680.000)

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
lines(c(-1,3),c(1.1E10,1.1E10),col = "black",lty=2)
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
