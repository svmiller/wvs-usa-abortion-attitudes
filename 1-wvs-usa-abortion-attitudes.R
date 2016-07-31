getwd()
# setwd("~/Dropbox/teaching/posc3410") # Note: change to a working directory good enough for you.
library(lattice)
library(RCurl)
library(Zelig)
library(lme4)

rm(list=ls())

# Read data

data <- getURL("https://raw.githubusercontent.com/svmiller/wvs-usa-abortion-attitudes/master/wvs-usa-abortion-attitudes-data.csv")
Data <- read.csv(text = data)
summary(Data)

histogram(Data$aj)
histogram(Data$ideology)
histogram(Data$godimportant)

M1 <- lm(aj ~ age + I(age^2)  + female + ideology + satisfinancial + cai  + godimportant, data=Data)
summary(M1)

Data$z_age <- with(Data, (age - mean(age, na.rm = TRUE))/(2*sd(age, na.rm = TRUE)))
Data$z_ideo <- with(Data, (ideology - mean(ideology, na.rm = TRUE))/(2*sd(ideology, na.rm = TRUE)))
Data$z_satisf <- with(Data, (satisfinancial - mean(satisfinancial, na.rm = TRUE))/(2*sd(satisfinancial, na.rm = TRUE)))
Data$z_cai <- with(Data, (cai - mean(cai, na.rm = TRUE))/(2*sd(cai, na.rm = TRUE)))
Data$z_god <- with(Data, (godimportant - mean(godimportant, na.rm = TRUE))/(2*sd(godimportant, na.rm = TRUE)))

M2 <- lm(aj ~ z_age + I(z_age^2)  + female + z_ideo + z_satisf + z_cai + z_god, data=Data)
summary(M2)

# Zelig is not playing nice with the z.age variable. Let's use interval age for now.
Data2 <- with(Data,  data.frame(aj, age, female, z_ideo, z_satisf, z_cai, z_god))
Data2 <- na.omit(Data2)

M3 <- zelig(aj ~ age + I(age^2) + female + z_ideo + z_satisf + z_cai + z_god, model = "ls", data = Data2)
summary(M3)

mingod <- min(Data2$z_god, na.rm = TRUE)
maxgod <- max(Data2$z_god, na.rm = TRUE)
minage <- min(Data2$age, na.rm = TRUE)
maxage <- max(Data2$age, na.rm = TRUE)

M3.low <- setx(M3, z_god = maxgod, age = minage:maxage)
M3.high <- setx(M3, z_god = mingod, age = minage:maxage)

M3.sim <- sim(M3, x = M3.low, x1 = M3.high)

ci.plot(M3.sim)

ci.plot(M3.sim, xlab = "Age in Years",
        ylab = "Expected Value on the Justifiability of Abortion",
        main = "Effect of Religion and Age on Attitudes toward Abortion",
)

M4 <- lmer(aj ~ z_age + I(z_age^2)  + female + z_ideo + z_satisf + z_cai + z_god + (1 | year), data = Data)
dotplot(ranef(M4, condVar=TRUE))
