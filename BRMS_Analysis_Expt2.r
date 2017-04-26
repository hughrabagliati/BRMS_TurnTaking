#!/exports/applications/apps/SL7/R/3.3.2/bin/Rscript

library(retimes)
library(rstan)
library(ggplot2)
library(retimes)
library(rstan)
library(doBy)
library(ggplot2)
library(brms)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

ads <- read.csv("exp2_adults.csv")
fives <- read.csv("exp2_fives.csv")
threes <- read.csv("exp2_threes.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

ads$Age1 <- "Adult"
fives$Age1 <- "Five"
threes$Age1 <- "Three"

 
tt.expt2 <- rbind(ads,threes,fives)
tt.expt2$Subject <- paste(tt.expt2$Age1,tt.expt2$Subject, sep = "")
tt.expt2$Age <- as.factor(tt.expt2$Age)

tt.expt2$rt_excl <- NA
for (i in levels(tt.expt2$Age)){
  tt.expt2[tt.expt2$Age == i,]$rt_excl <- (tt.expt2[tt.expt2$Age == i,]$RT.ms - mean(tt.expt2[tt.expt2$Age == i,]$RT.ms))/(sd(tt.expt2[tt.expt2$Age == i,]$RT.ms))
}
tt.expt2<- subset(tt.expt2, rt_excl <= 5000 & rt_excl >= -1.5)


summaryBy(RT.ms ~ Subject, data = tt.expt2, FUN = length) -> n.trials
tt.expt2 <- subset(tt.expt2, Subject %in% n.trials[n.trials$RT.ms.length >= 20,]$Subject & Accuracy == 1)

tt.expt2$rt <- (tt.expt2$RT.ms - mean(tt.expt2$RT.ms))/(2.5*sd(tt.expt2$RT.ms))
tt.expt2$rt <- tt.expt2$rt + abs(min(tt.expt2$rt))+0.1


expt2.formula <- brmsformula(rt ~ 1  +Age  * Early.Late * Pred+(1+Early.Late|Subject) + (1|CharacterLength.ms), 
                             beta ~ 1  +Age  * Early.Late * Pred+(1+Early.Late|Subject)  + (1|CharacterLength.ms), 
                             sigma ~ 1  +Age * Early.Late * Pred+(1+Early.Late|Subject)+ (1|CharacterLength.ms))
my_family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log")


expt2.formula.noage <- brmsformula(rt ~ 1  + Early.Late * Pred+(1+Early.Late|Subject) + (1|CharacterLength.ms), 
                             beta ~ 1  + Early.Late * Pred+(1+Early.Late|Subject)  + (1|CharacterLength.ms), 
                             sigma ~ 1  + Early.Late * Pred+(1+Early.Late|Subject)+ (1|CharacterLength.ms))
expt2.formula.noage.nocorr <- brmsformula(rt ~ 1  + Early.Late * Pred+(1+Early.Late||Subject) + (1|CharacterLength.ms), 
                                   beta ~ 1  + Early.Late * Pred+(1+Early.Late||Subject)  + (1|CharacterLength.ms), 
                                   sigma ~ 1  + Early.Late * Pred+(1+Early.Late||Subject)+ (1|CharacterLength.ms))

expt2.formula.noraneff <- brmsformula(rt ~ 1  +Age  * Early.Late * Pred+(1|Subject) + (1|CharacterLength.ms), 
                             beta ~ 1  +Age  * Early.Late * Pred+(1|Subject)  + (1|CharacterLength.ms), 
                             sigma ~ 1  +Age * Early.Late * Pred+(1|Subject)+ (1|CharacterLength.ms))

expt2.formula.noage.noraneff <- brmsformula(rt ~ 1  + Early.Late * Pred+(1|Subject) + (1|CharacterLength.ms), 
                                   beta ~ 1  + Early.Late * Pred+(1|Subject)  + (1|CharacterLength.ms), 
                                   sigma ~ 1  + Early.Late * Pred+(1|Subject)+ (1|CharacterLength.ms))


my_family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log")
my_prior = set_prior("normal(0,1)", class ="b")




expt2.model <- brm(expt2.formula, data = tt.expt2, family = my_family, chains = 4, iter = 3000, warmup = 1800, inits="0")

expt2.model.adult <- brm(expt2.formula.noage, data = subset(tt.expt2, Age == "Adult"), family = my_family, chains = 4, iter = 3000, warmup = 1800, inits="0")

expt2.model.five <- brm(expt2.formula.noage, data = subset(tt.expt2, Age == "Five"), family = my_family, chains = 4, iter = 6000, warmup = 3800, inits="0")

expt2.model.three <- brm(expt2.formula.noage, data = subset(tt.expt2, Age == "Three"), family = my_family, chains = 4, iter = 6000, warmup = 3800, inits="0")


save(expt2.model, expt2.model.adult,expt2.model.five,expt2.model.three, file = "brms_expt2_fineage.RDATA")


ads <- read.csv("exp2_adults.csv")
fives <- read.csv("exp2_fives.csv")
threes <- read.csv("exp2_threes.csv")

ads$Age <- "Adult"
fives$Age <- "Child"
threes$Age <- "Child"

ads$Age1 <- "Adult"
fives$Age1 <- "Five"
threes$Age1 <- "Three"


tt.expt2 <- rbind(ads,threes,fives)
tt.expt2$Subject <- paste(tt.expt2$Age1,tt.expt2$Subject, sep = "")
tt.expt2$Age <- as.factor(tt.expt2$Age)

tt.expt2$rt_excl <- NA
for (i in levels(tt.expt2$Age)){
  tt.expt2[tt.expt2$Age == i,]$rt_excl <- (tt.expt2[tt.expt2$Age == i,]$RT.ms - mean(tt.expt2[tt.expt2$Age == i,]$RT.ms))/(sd(tt.expt2[tt.expt2$Age == i,]$RT.ms))
}
tt.expt2<- subset(tt.expt2, rt_excl <= 5000 & rt_excl >= -2.5)
# Note that the child model wouldn't converge unless we excluded anticipations 
# that were < 1.8 SD from the mean (that means anticipations ~ 800ms)

summaryBy(RT.ms ~ Subject, data = tt.expt2, FUN = length) -> n.trials
tt.expt2 <- subset(tt.expt2, Subject %in% n.trials[n.trials$RT.ms.length >= 20,]$Subject & Accuracy == 1)

tt.expt2$rt <- (tt.expt2$RT.ms - mean(tt.expt2$RT.ms))/(sd(tt.expt2$RT.ms))
tt.expt2$rt <- tt.expt2$rt + abs(min(tt.expt2$rt))+0.1

# We have two large outliers (each separated from prior values by > 2.5sd)
# Model doesn't converge if we don't remove
tt.expt2 <- subset(tt.expt2, rt < 14)
expt2.model.grossage <- brm(expt2.formula, data = tt.expt2, family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")
expt2.model.adult.noage <- brm(expt2.formula.noage, data = subset(tt.expt2, Age == "Adult"), family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")

expt2.model.child.noage <- brm(expt2.formula.noage, data = subset(tt.expt2, Age != "Adult"), family = my_family, chains = 4, iter = 4000, warmup = 2400, inits="0")

save(expt2.model.child.noage, expt2.model.adult.noage, expt2.model.grossage, file = "brms_expt2_grossage.RDATA")

expt2.model.grossage <- brm(expt2.formula.noraneff, data = tt.expt2, family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0")
expt2.model.adult.noage <- brm(expt2.formula.noage.noraneff, data = subset(tt.expt2, Age == "Adult"), family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0")

expt2.model.child.noage <- brm(expt2.formula.noage.noraneff, data = subset(tt.expt2, Age != "Adult"), family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0")

save(expt2.model.child.noage, expt2.model.adult.noage, expt2.model.grossage, file = "brms_expt2_grossage_noraneff.RDATA")


####
# Include length


contrasts(tt.expt2$Length)[1] <- -1
contrasts(tt.expt2$Age)[1] <- -1
contrasts(tt.expt2$Early.Late)[1] <- -1
contrasts(tt.expt2$Pred)[1] <- -1

expt2.formula.noraneff.length <- brmsformula(rt ~ 1 +Length +Age  * Early.Late * Pred+(1|Subject) + (1|CharacterLength.ms), 
                                      beta ~ 1  +Length+Age  * Early.Late * Pred+(1|Subject)  + (1|CharacterLength.ms), 
                                      sigma ~ 1  +Length+Age * Early.Late * Pred+(1|Subject)+ (1|CharacterLength.ms))

expt2.formula.noage.noraneff.length <- brmsformula(rt ~ 1  +Length+ Early.Late * Pred+(1|Subject) + (1|CharacterLength.ms), 
                                            beta ~ 1  +Length+ Early.Late * Pred+(1|Subject)  + (1|CharacterLength.ms), 
                                            sigma ~ 1  +Length+ Early.Late * Pred+(1|Subject)+ (1|CharacterLength.ms))

tt.expt2 <- subset(tt.expt, RT.ms > -500)

expt2.model.grossage.length <- brm(expt2.formula.noraneff.length, data = tt.expt2, family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0", prior = my_prior)
expt2.model.adult.noage.length <- brm(expt2.formula.noage.noraneff.length, data = subset(tt.expt2, Age == "Adult"), family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0", prior = my_prior)

expt2.model.child.noage.length <- brm(expt2.formula.noage.noraneff.length, data = subset(tt.expt2, Age != "Adult"), family = my_family, chains = 4, iter = 2000, warmup = 1200, inits="0", prior = my_prior)

save(expt2.model.child.noage.length, expt2.model.adult.noage.length, expt2.model.grossage.length, file = "brms_expt2_grossage_noraneff_length_home.RDATA")
