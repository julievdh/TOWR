## Determine which tows have significant effect of depth on drag, using AIC approach

## set directory
setwd("~/Documents/R/TOWR")


## load libraries
library(AICcmodavg)

## find all filenames
files <- list.files(path = "./TOW_data", pattern = ".csv")

## initialize AIC storage vectors
# AICc_1 <- numeric(length(files))
# AICc_2 <- numeric(length(files))
delAIC <-numeric(length(files))

## for each file
for(i in 1:length(files)){
  ## load data
  setwd("~/Documents/R/TOWR/TOW_data")
dat <- read.csv(files[i],header=FALSE)

mndepth <- dat$V1
mnspeed <- dat$V2
mndragN <- dat$V3
speedcat <- c(1,2,3,1,2,3,1,2,3)

## plot
# par(new = T)
# plot(mndepth,mndragN, pch=19,col = speedcat, xlim=c(0.0,12.0), ylim=c(0.0,700))

# fit model: drag ~ speed^2
m1 <-lm(mndragN ~ mnspeed^2)
summary(m1)
# plot(m1)

# fit model: drag ~ speed^2 + depth
m2 <-lm(mndragN ~ mnspeed^2 + mndepth)
summary(m2)
# plot(m2)

# calculate AIC
#AICc_1[[i]] <- AICc(m1)
#AICc_2[[i]] <- AICc(m2)
# delAICc[[i]] <- AICc(m2) - AICc(m1)
delAIC[[i]] <- AIC(m2) - AIC(m1)
}


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

