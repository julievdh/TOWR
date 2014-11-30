## Determine which tows have significant effect of depth on drag, using AIC approach

## set directory
setwd("~/Documents/R/TOWR/TOW_data")

## load libraries
library(AICcmodavg)
library(R.matlab)

## load gear data saved from MATLAB
data <- readMat('GearData.mat')


## initialize AIC storage vectors
best <-numeric(21)
m3p <-numeric(21)

## for each file
for(i in 1:21) {

## plot
plot(log(data$Cd[,i]) ~ data$Re[,i])

# fit model: drag ~ speed^2
m1 <-lm(log(data$Cd[,i]) ~ data$Re[,i])
summary(m1)
# plot(m1)

# fit model: drag ~ speed^2 + depth
m2 <-lm(data$Cd[,i] ~ data$Re[,i] + data$depth[,i])
summary(m2)
# plot(m2)

m3 <- lm(data$Cd[,i] ~ data$depth[,i])
m3p[i] <- anova(m3)$'Pr(>F)'[1]

# calculate del AIC
x <- c(AIC(m1), AIC(m2), AIC(m3)) # store AIC values in a vector
delta <- x - min(x)               # calculate AIC differences
best[i] <- match(0,delta)        # find which model is preferred (delta = 0)
}

model2 <- which(best %in% c(2))
model3 <- which(best %in% c(3))

# find where delta AIC < -2 
# this will be where model 2 is preferred (where depth should be included)
# Arnold, T. W. 2010. Uninformative parameters and model selection using Akaike’s Information Criterion. Journal of Wildlife Management 74:1175–1178. 
# We usually consider models within 2 delta AIC as competitive. However, if a model has an addition of only one parameter to its competitor and that 
# parameter is not significant, that parameter is likely spurious. AIC = –2LL + 2K so the penalty for adding one parameter is +2 AIC. If only one 
# parameter is added but the AIC is within 2 delta AIC, the model fit was not improved enough to overcome the penalty. Therefore, that parameter 
# is uninformative and should not be included in the model or interpreted as having an effect.

dpth <- files[delAIC< -2]

# and plot them 
par(mfrow = c(2,4))

for(i in 1:length(dpth)){
setwd("~/Documents/R/TOWR/TOW_data")
dat <- read.csv(dpth[i],header=FALSE)

mndepth <- dat$V1
mnspeed <- dat$V2
mndragN <- dat$V3
speedcat <- c(1,2,3,1,2,3,1,2,3)

## plot mean depth and mean drag, coloured by speed
plot(mndepth,mndragN, pch=19,col = speedcat, main = dpth[i])

## plot mean speed and mean drag, coloured by depth
#plot(mnspeed,mndragN, pch=19, col = mndepth+1, main = dpth[i])

}

