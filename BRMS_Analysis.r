
library(retimes)
library(rstan)
library(ggplot2)
library(doBy)
library(brms)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

ads <- read.csv("Adult_R.csv")
fives <- read.csv("5yo_R.csv")
threes <- read.csv("3yo_R.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

ads$Age1 <- "Adult"
fives$Age1 <- "Five"
threes$Age1 <- "Three"


tt.expt1 <- rbind(ads,fives,threes)
tt.expt1$Age <- as.factor(tt.expt1$Age)
tt.expt1$Subject <- paste(tt.expt1$Age1,tt.expt1$Participant, sep = "")

tt.expt1$rt_excl <- NA
for (i in levels(tt.expt1$Age)){
  tt.expt1[tt.expt1$Age == i,]$rt_excl <- (tt.expt1[tt.expt1$Age == i,]$RTms - mean(tt.expt1[tt.expt1$Age == i,]$RTms))/(sd(tt.expt1[tt.expt1$Age == i,]$RTms))
}
tt.expt1<- subset(tt.expt1, rt_excl <= 5000 & rt_excl >= -2.5)

summaryBy(RTms ~ Subject, data = tt.expt1, FUN = length) -> n.trials
tt.expt1 <- subset(tt.expt1, Subject %in% n.trials[n.trials$RTms.length >= 20,]$Subject & Incorrect. == "")


#tt<- subset(tt, RTms <= 5000 & RTms >= -500)
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt.expt1$rt <- (tt.expt1$RTms - mean(tt.expt1$RTms))/(sd(tt.expt1$RTms))
tt.expt1$rt <- tt.expt1$rt + abs(min(tt.expt1$rt))+0.1

expt1.formula <- brmsformula(rt ~ 1  +Age*Match*Pred+(1+Match*Pred|Subject) + (1|Time.to.Say.Character.Name), 
                         beta ~ 1  +Age*Match*Pred+(1+Match*Pred|Subject)  + (1|Time.to.Say.Character.Name), 
                         sigma ~ 1  +Age*Match*Pred+(1+Match*Pred|Subject)+ (1|Time.to.Say.Character.Name))
expt1.formula.noage <- brmsformula(rt ~ 1  +Match*Pred+(1+Match*Pred|Subject) + (1|Time.to.Say.Character.Name), 
                                   beta ~ 1  +Match*Pred+(1+Match*Pred|Subject)  + (1|Time.to.Say.Character.Name), 
                                   sigma ~ 1  +Match*Pred+(1+Match*Pred|Subject)+ (1|Time.to.Say.Character.Name))

expt1.formula.noage.nocorr <- brmsformula(rt ~ 1  +Match*Pred+(1+Match*Pred||Subject) + (1|Time.to.Say.Character.Name), 
                                   beta ~ 1  +Match*Pred+(1+Match*Pred||Subject)  + (1|Time.to.Say.Character.Name), 
                                   sigma ~ 1  +Match*Pred+(1+Match*Pred||Subject)+ (1|Time.to.Say.Character.Name))


expt1.formula.noranef <- brmsformula(rt ~ 1  +Age*Match*Pred+(1|Subject) + (1|Time.to.Say.Character.Name), 
                             beta ~ 1  +Age*Match*Pred+(1|Subject)  + (1|Time.to.Say.Character.Name), 
                             sigma ~ 1  +Age*Match*Pred+(1|Subject)+ (1|Time.to.Say.Character.Name))
expt1.formula.noage.noranef <- brmsformula(rt ~ 1  +Match*Pred+(1|Subject) + (1|Time.to.Say.Character.Name), 
                                   beta ~ 1  +Match*Pred+(1|Subject)  + (1|Time.to.Say.Character.Name), 
                                   sigma ~ 1  +Match*Pred+(1|Subject)+ (1|Time.to.Say.Character.Name))


my_family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log")
expt1.fullage <- brm(expt1.formula.noranef, data = tt.expt1, family = my_family, chains = 4, iter = 1600, warmup = 800, inits="0")
expt1.fullage.adult <- brm(expt1.formula.noage.noranef, data = subset(tt.expt1,Age == "Adult"), family = my_family, chains = 4, iter = 1600, warmup = 800, inits="0")
expt1.fullage.five <- brm(expt1.formula.noage.noranef, data = subset(tt.expt1,Age == "Five"), family = my_family, chains = 4, iter = 1600, warmup = 800, inits="0")
expt1.fullage.three <- brm(expt1.formula.noage.noranef, data = subset(tt.expt1,Age == "Three"), family = my_family, chains = 4, iter = 1600, warmup = 800, inits="0")

save(expt1.fullage,expt1.fullage.adult,expt1.fullage.five,expt1.fullage.three, file = "Expt1_EachAge.RDATA")

######
# Gross age analysis
ads <- read.csv("Adult_R.csv")
fives <- read.csv("5yo_R.csv")
threes <- read.csv("3yo_R.csv")

ads$Age <- "Adult"
fives$Age <- "Child"
threes$Age <- "Child"

ads$Age1 <- "Adult"
fives$Age1 <- "Five"
threes$Age1 <- "Three"


tt.expt1 <- rbind(ads,fives,threes)
tt.expt1$Age <- as.factor(tt.expt1$Age)
tt.expt1$Subject <- paste(tt.expt1$Age1,tt.expt1$Participant, sep = "")

tt.expt1$rt_excl <- NA
for (i in levels(tt.expt1$Age)){
  tt.expt1[tt.expt1$Age == i,]$rt_excl <- (tt.expt1[tt.expt1$Age == i,]$RTms - mean(tt.expt1[tt.expt1$Age == i,]$RTms))/(sd(tt.expt1[tt.expt1$Age == i,]$RTms))
}
tt.expt1<- subset(tt.expt1, rt_excl <= 5000 & rt_excl >= -2.5)

summaryBy(RTms ~ Subject, data = tt.expt1, FUN = length) -> n.trials
tt.expt1 <- subset(tt.expt1, Subject %in% n.trials[n.trials$RTms.length >= 20,]$Subject & Incorrect. == "")

#tt<- subset(tt, RTms <= 5000 & RTms >= -500)
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt.expt1$rt <- (tt.expt1$RTms - mean(tt.expt1$RTms))/(sd(tt.expt1$RTms))
tt.expt1$rt <- tt.expt1$rt + abs(min(tt.expt1$rt))+0.1

expt1.grossage <- brm(expt1.formula, data = tt.expt1, family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")
expt1.grossage.adult <- brm(expt1.formula.noage, data = subset(tt.expt1,Age == "Adult"), family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")
expt1.grossage.child <- brm(expt1.formula.noage.nocorr, data = subset(tt.expt1,Age == "Child"), family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")

save(expt1.grossage,expt1.grossage.adult,expt1.grossage.child, file = "expt1_brms_1sd.RDATA")

expt1.grossage <- brm(expt1.formula.noranef, data = tt.expt1, family = my_family, chains = 4, iter = 2000, warmup = 1400, inits="0")
expt1.grossage.adult <- brm(expt1.formula.noage.noranef, data = subset(tt.expt1,Age == "Adult"), family = my_family, chains = 4, iter = 2000, warmup = 1400, inits="0")
expt1.grossage.child <- brm(expt1.formula.noage.noranef, data = subset(tt.expt1,Age == "Child"), family = my_family, chains = 4, iter = 2000, warmup = 1400, inits="0")

save(expt1.grossage,expt1.grossage.adult,expt1.grossage.child, file = "expt1_brms_1sd.noranef.RDATA")