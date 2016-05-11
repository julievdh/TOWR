library(R.matlab)
library(agricolae)

## set directory and load data
setwd("~/Documents/MATLAB/Eg4057")
dat <- readMat("allCT.mat") # Data from Feb 15 2016
setwd("~/Documents/R")

# set up factors
Condition <- as.factor(dat$condition)
Phase <- as.factor(dat$portion)

CT.linear.model <- lm(dat$allCT ~ Condition + Phase)
anova(CT.linear.model)

ni.linear.model <- lm(dat$allni ~ Condition + Phase)
anova(ni.linear.model)

st.linear.model <- lm(dat$allst ~ Condition + Phase)
anova(st.linear.model)

eta.linear.model <- lm(dat$alleta ~ Condition + Phase)
anova(eta.linear.model)

pt.linear.model <- lm(dat$all.pt ~ Condition + Phase)
anova(pt.linear.model)