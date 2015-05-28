# Energy Consumption Differences

# maximum difference in energy consumption
# From calculations 7 April 2015
# Based on basic timeline
# UNITS ARE JOULES
diff_max_basic <- c(6.2547E+09,2.7669E+10,3.7636E+11,2.1070E+10,2.0811E+09,1.4285E+10,
              2.7023E+10,7.1041E+10,2.7254E+10,1.9780E+11,9.5343E+09,1.6175E+10,
              3.2701E+10,1.4899E+11,4.0010E+11)
diff_min_basic <- c(1.3697E+08,2.4258E+10,7.6482E+09,7.1930E+08,1.6359E+08,2.7426E+09,
              4.6296E+09,7.3037E+08,2.6152E+10,1.5444E+10,2.4332E+09,4.4259E+09,1.2287E+09,
              2.6544E+10,1.2023E+10)

# Based on detailed timeline
# UNITS ARE JOULES
diff_max_detailed <- c(6.2547E+09,2.4313E+10,2.7920E+11,2.0486E+10,2.0779E+09,1.2923E+10,
                       2.5645E+10,6.9492E+10,2.7005E+10,4.0779E+10,9.9170E+09,1.6439E+10,
                       3.2452E+10,1.5288E+11,4.8754E+11)
diff_min_detailed<- c(1.3597E+08,2.2977E+10,7.6473E+09,1.2036E+09,2.8166E+08,2.8219E+09,
                      4.9594E+09,1.0498E+09,2.6152E+10,3.2797E+09,2.8529E+09,4.7788E+09,
                      1.2678E+09,2.7832E+10,1.5835E+10)

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

beanplot(diff_min_detailed ~ fate,ll = 0.25, log="y", method = "stack",
         side = "both" , ylab="Additional Work (J)",axes = FALSE, ylim = c(1E7,1E13),
         col = list("white", c("grey", "black")), beanlinewd = 1.75)
# text(1,1E13,'A',cex = 1.2)
axis(2,at=c(1E7,1E8,1E9,1E10,1E11,1E12),labels = c(expression(paste("10"^"7")),
                                              expression(paste("10"^"8")),
                                              expression(paste("10"^"9")),
                                              expression(paste("10"^"10")),
                                              expression(paste("10"^"11")),
                                              expression(paste("10"^"12"))))

beanplot(diff_max_detailed ~ fate,ll = 0.25, log="y",names = c("",""),
         side = "both", axes = FALSE, ylim = c(1E7,1E13),method = "stack",
         col = list("white", c("grey", "black")), beanlinewd = 1.75)
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
