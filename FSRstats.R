library(R.matlab)
library(agricolae)

## set directory and load data
setwd("~/Documents/MATLAB/Eg4057")
dat <- readMat("FSRvars.mat")
setwd("~/Documents/R")

# set up factors
Ind <- as.factor(dat$indv)
Condition <- as.factor(dat$cond)
Phase <- as.factor(dat$phase.ifi)

ifi.linear.model <- lm(dat$ifi.all ~ Ind + Condition + Phase)
ifi.aov <- anova(ifi.linear.model)

hz.linear.model <- lm(dat$hz.all ~ Ind + Condition + Phase)
hz.aov <- anova(hz.linear.model)

TukeyHSD(aov(ifi.linear.model))
TukeyHSD(aov(hz.linear.model))

# plot
plot(dat$ifi.all ~ Ind*Condition*Phase)
plot(dat$hz.all ~ Ind*Condition*Phase)
