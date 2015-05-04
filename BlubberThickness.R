# Are blubber thicknesses significantly different between entangled 
# and nonentangled, within life stages?

library(R.matlab)
library(agricolae)

setwd("~/Documents/MATLAB/TOW/Blubber")
data <- readMat('DorsalThickness.mat')

# Rename for ease
Dorsal <- data$Dorsal # Dorsal blubber thicknesses
Sex <- c(0,0,1,0,0,1,0,0,0,1,0,1,0,1,0,0,1,1,0,1,1,55,1,0,1,1,1,0,1,1,0)

# Entanglement and stage as factors
Ent <- (c(rep(0,22),rep(1,9))) # Entangled status. 0 = not, 1 = ent
Stage <- c(rep(0,9),rep(1,7),rep(2,6),rep(0,2),rep(1,4),rep(2,3)) # 0 = calf, 1 = juv, 3 = adult

# set up linear model
linear.model <- lm(Dorsal ~ Ent + factor(Stage) + factor(Sex))
aov1 <- anova(linear.model)

linear.model_nocalves <- lm(Dorsal[Stage > 0] ~ Ent[Stage > 0] + factor(Stage[Stage > 0]))
aov2 <- anova(linear.model_nocalves)

# don't need Tukey as none have > 2 factors
# TukeyHSD(aov(Dorsal[Stage > 0] ~ factor(Ent[Stage > 0]) + factor(Stage[Stage > 0]) + factor(Sex[Stage > 0])))

# PLOT
# load required libraries
require(vioplot)
require(devtools)
require(digest)
require(beanplot)

# set directory for pdf
setwd("~/Documents/R/TOWR/AnalysisFigures") # set directory to figures
op <- par

pdf("BlubberThickness_beanplot.pdf",width = 5,height = 5)

beanplot(Dorsal[Stage > 0] ~ Ent[Stage > 0] + Stage[Stage > 0],ll = 0.20,
         side = "both" , ylab="Dorsal Blubber Thickness (cm)", names = c("Juveniles","Adults"),
         col = list("white", c("grey", "black")), beanlinewd = 1.5)
dev.off()
par(new)

pdf("BlubberThickness_beanplot2.pdf",width = 5,height = 5)

beanplot(Dorsal[Stage > 0] ~ Ent[Stage > 0],ll = 0.07,
         side = "both" , ylab="Dorsal Blubber Thickness (cm)", 
         col = list("white", c("grey", "black")), beanlinewd = 1.5)


par(op)
dev.off()


