library(R.matlab)
library(agricolae)
library(dae)

## set directory and load data
setwd("~/Documents/MATLAB/Eg4057")
dat <- readMat("FSRvars.mat") ## Data updated and resaved Feb 17 2016
setwd("~/Documents/R")

# set up factors
Ind <- as.factor(dat$indv)
Condition <- as.factor(dat$cond)
Phase <- as.factor(dat$phase.hz)

hz.linear.model <- lm(dat$hz.all ~ Ind+Condition+Phase)
hz.aov <- anova(hz.linear.model)

TukeyHSD(aov(hz.linear.model))

# plot
df = data.frame(Ind, Condition, Phase, dat$hz.all)     # create data frame
df = na.omit(df)                             # remove NAs
interaction.ABC.plot(dat.hz.all, Phase, Condition, Ind, data = df)
boxplot(dat$hz.all ~ Condition*Phase,at =c(1,2, 4,5, 7,8, 10,11))
plot(dat$hz.all ~ Ind*Condition*Phase)

## load duration data
setwd("~/Documents/MATLAB/Eg4057")
dat <- readMat("FSdur.mat") 
setwd("~/Documents/R")

# concatenate
dur <- c(dat$dur.highasc.3911,dat$dur.highasc.4057,dat$dur.highbot.3911,dat$dur.highbot.4057,
         dat$dur.highdesc.3911,dat$dur.highdesc.4057,dat$dur.highsurf.3911,dat$dur.highsurf.4057,
         dat$dur.lowasc.3911,dat$dur.lowasc.4057,dat$dur.lowbot.3911,dat$dur.lowbot.4057,
         dat$dur.lowdesc.3911,dat$dur.lowdesc.4057,dat$dur.lowsurf.3911,dat$dur.lowsurf.4057)
ind <- c(rep(3911,271*53),rep(4057,616*6),rep(3911,242*53),rep(4057,504*6),
         rep(3911,64*53),rep(4057,116*6),rep(3911,201*52),rep(4057,441*5),
         rep(3911,233*101),rep(4057,387*11),rep(3911,198*101),rep(4057,267*11),
         rep(3911,57*101),rep(4057,158*11),rep(3911,88*100),rep(4057,308*10))
cond <- c(rep(1,271*53),rep(1,616*6),rep(1,242*53),rep(1,504*6),
          rep(1,64*53),rep(1,116*6),rep(1,201*52),rep(1,441*5),
          rep(0,233*101),rep(0,387*11),rep(0,198*101),rep(0,267*11),
          rep(0,57*101),rep(0,158*11),rep(0,88*100),rep(0,308*10))
phase <- c(rep(1,271*53),rep(1,616*6),rep(0,242*53),rep(0,504*6),
           rep(-1,64*53),rep(-1,116*6),rep(2,201*52),rep(2,441*5),
           rep(1,233*101),rep(1,387*11),rep(0,198*101),rep(0,267*11),
           rep(-1,57*101),rep(-1,158*11),rep(2,88*100),rep(2,308*10))

# set up factors
Ind <- as.factor(ind)
Condition <- as.factor(cond)
Phase <- as.factor(phase)

dur.linear.model <- lm(dur ~ Ind*Condition*Phase)
dur.aov <- anova(dur.linear.model)

tukdur <-TukeyHSD(aov(dur.linear.model))

plot(dur ~ Ind*Condition*Phase)

# plot all individuals, conditions, phases
boxplot(dur ~ Ind*Condition*Phase, las = 2, # changes axis label direction
        at =c(1,2,3,4, 6,7,8,9, 11,12,13,14, 16,17,18,19),
        ylab = "Duration (sec)")

# plot separated by individuals
# COULD MAKE PRETTIER LOOK AT THIS: 
# https://jpwendler.wordpress.com/2013/05/21/reordering-the-factor-levels-in-r-boxplots-and-making-them-look-pretty-with-base-graphics/
boxplot(dur ~ Condition*Phase*Ind, las = 2, # changes axis label direction
        at =c(1,2, 4,5, 7,8, 10,11, 13,14, 16,17, 19, 20, 22,23),
        ylab = "Duration (sec)")

# three-way interaction plot
df = data.frame(Ind, Condition, Phase, dur)     # create data frame
subdf = na.omit(df)                             # remove NAs
# plot main effect of condition
interaction.ABC.plot(dur, Condition, Phase, Ind, data = subdf,
                     ylab = "Duration (sec)", scales(list(x = tick.number(0,1))))
# plot main effect of phase
interaction.ABC.plot(dur, Phase, Condition, Ind, data = subdf)


# create two separate data frames for each individual
## 3911 Plots
subdf3911 = subdf[subdf$Ind == '3911',]


## 4057 Plots
subdf4057 = subdf[subdf$Ind == '4057',]
par(mfrow=c(2,1))
interaction.plot(subdf4057$Phase, subdf4057$Condition, subdf4057$dur,
                 xlab = "Dive Phase",
                 ylab = "Mean Duration (sec)",
                 trace.label = "Condition")
interaction.plot(subdf4057$Condition, subdf4057$Phase, subdf4057$dur,
                 xlab = "Condition",
                 ylab = "Mean Duration (sec)",
                 trace.label = "Dive Phase")