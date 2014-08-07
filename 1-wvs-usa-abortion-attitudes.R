getwd()
setwd("/Dropbox/teaching/posc3410") # Note: change to a working directory good enough for you.
library(car)
library(lattice)
library(RCurl)
library(Zelig)
library(lme4)

rm(list=ls())

## Read the data from Github. Note, I wish I could still do this from Dropbox.
##############################################################################

data <- getURL("https://raw.githubusercontent.com/svmiller/wvs-usa-abortion-attitudes/master/wvs-usa-abortion-attitudes-data.csv")
Data <- read.csv(text = data)
summary(Data)

histogram(Data$aj)
histogram(Data$ideology)
histogram(Data$godimportant)

M1 <- lm(aj ~ age + I(age^2)  + female + unemployed + ideology + satisfinancial + cai + trustmostpeople + godimportant + respectauthority + nationalpride, data=Data)
summary(M1)

Data$z.age <- with(Data, (age - mean(age, na.rm = TRUE))/(2*sd(age, na.rm = TRUE)))
Data$z.ideology <- with(Data, (ideology - mean(ideology, na.rm = TRUE))/(2*sd(ideology, na.rm = TRUE)))
Data$z.satisfinancial <- with(Data, (satisfinancial - mean(satisfinancial, na.rm = TRUE))/(2*sd(satisfinancial, na.rm = TRUE)))
Data$z.godimportant <- with(Data, (godimportant - mean(godimportant, na.rm = TRUE))/(2*sd(godimportant, na.rm = TRUE)))

M2 <- lm(aj ~ z.age + I(z.age^2)  + female + unemployed + z.ideology + z.satisfinancial + cai + trustmostpeople + z.godimportant + respectauthority + nationalpride, data=Data)

# Zelig is not playing nice with the z.age variable. Let's use interval age for now.
Data2 <- with(Data,  data.frame(aj, age, female, unemployed, z.ideology, z.satisfinancial, cai, trustmostpeople, z.godimportant, respectauthority, nationalpride))
Data2 <- na.omit(Data2)

M3 <- zelig(aj ~ age + I(age^2)  + female + unemployed + z.ideology + z.satisfinancial + cai + trustmostpeople + z.godimportant + respectauthority + nationalpride, model = "ls", data = Data2)

mingod <- min(Data2$z.godimportant, na.rm = TRUE)
maxgod <- max(Data2$z.godimportant, na.rm = TRUE)
minage <- min(Data2$age, na.rm = TRUE)
maxage <- max(Data2$age, na.rm = TRUE)

M3.low <- setx(M3, z.godimportant = maxgod, age = minage:maxage)
M3.high <- setx(M3, z.godimportant = mingod, age = minage:maxage)

M3.sim <- sim(M3, x = M3.low, x1 = M3.high)

plot.ci(M3.sim)

plot.ci(M3.sim, xlab = "Age in Years",
         ylab = "Expected Value on the Justifiability of Abortion",
         main = "Effect of Religion and Age on Attitudes toward Abortion",
)

M4 <- lmer(aj ~ z.age + I(z.age^2)  + female + unemployed + z.ideology + z.satisfinancial + cai + trustmostpeople + z.godimportant + respectauthority + nationalpride + (1 | wave), data = Data)
dotplot(ranef(M4, condVar=TRUE))

M5 <- zelig(aj ~ z.age + I(z.age^2) + female + unemployed + z.ideology + z.satisfinancial + cai + trustmostpeople + z.godimportant + respectauthority + nationalpride, model = "gamma", data = Data)
