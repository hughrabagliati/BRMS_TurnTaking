#!/exports/applications/apps/SL7/R/3.3.2/bin/Rscript

library(retimes)
library(rstan)
library(ggplot2)
library(retimes)
library(rstan)
library(doBy)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

ads <- read.csv("exp2_adults.csv")
fives <- read.csv("exp2_fives.csv")
threes <- read.csv("exp2_threes.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

tt <- rbind(ads,threes,fives)
tt$Subject <- paste(tt$Age,tt$Subject, sep = "")
tt$Age <- as.factor(tt$Age)
tt$OmniAge <- as.factor(ifelse(tt$Age == "Adult","Adult","Kid"))

tt$rt_excl <- NA
for (i in levels(tt$Age)){
  tt[tt$Age == i,]$rt_excl <- (tt[tt$Age == i,]$RT.ms - mean(tt[tt$Age == i,]$RT.ms))/(sd(tt[tt$Age == i,]$RT.ms))
}

tt$AllTrialNo <- 1
for (i in unique(tt$Subject)){
  tt[tt$Subject == i & tt$Early.Late == "early",]$AllTrialNo <-1:length(tt[tt$Subject == i & tt$Early.Late == "early",]$AllTrialNo)
}

tt<- subset(tt, rt_excl <= 5000 & rt_excl >= -1.5)
#tt<- subset(tt, RT.ms <= 5000 & RT.ms >= -500)

contrasts <- data.frame(model.matrix(~Age * Pred * Early.Late , data = tt, contrasts.arg = list(Age = "contr.sum", 
                                                                                                Pred = "contr.sum",Early.Late = "contr.sum")))


# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt$rt <- tt$RT.ms
tt$N_Early <- contrasts$Early.Late1
tt$N_Pred <- contrasts$Pred1
tt$N_AgeFive <- contrasts$Age1
tt$N_AgeThree <- contrasts$Age2
tt$N_E_P_Interact <- contrasts$Pred1.Early.Late1
tt$N_Early_AgeFive_Interact <-  contrasts$Age1.Early.Late1
tt$N_Early_AgeThree_Interact <- contrasts$Age2.Early.Late1 
tt$N_Pred_AgeFive_Interact <- contrasts$Age1.Pred1
tt$N_Pred_AgeThree_Interact <- contrasts$Age2.Pred1
tt$N_Early_Pred_AgeFive_Interact <- contrasts$Age1.Pred1.Early.Late1
tt$N_Early_Pred_AgeThree_Interact <- contrasts$Age2.Pred1.Early.Late1
# tt$rt_scale <- (tt$rt - mean(tt$rt,na.rm = T))/sd(tt$rt, na.rm = T)


cols = c("N_Early","N_Pred","N_AgeFive","N_AgeThree","N_E_P_Interact","N_Early_AgeFive_Interact",
         "N_Pred_AgeFive_Interact","N_Early_Pred_AgeFive_Interact",
         "N_Early_AgeThree_Interact","N_Pred_AgeThree_Interact","N_Early_Pred_AgeThree_Interact")
for (i in unique(cols)){
  tt[,i] <- ifelse(tt[,i] == -1, -0.5, ifelse(tt[,i] == 1,0.5,0))
}

summaryBy(rt ~ Subject, data = tt, FUN = length) -> n.trials
tt <- subset(tt, Subject %in% n.trials[n.trials$rt.length >= 20,]$Subject & Accuracy == 1)
tt$rt <- tt$rt + abs(min(tt$rt)) + 0.001

tt$rt <- (tt$rt - mean(tt$rt))/(sd(tt$rt))
tt$scale_character_length <- (tt$CharacterLength.ms - mean(tt$CharacterLength.ms))/(sd(tt$CharacterLength.ms))

stanDat_full <- list(rt = tt$rt,
                     factor1 = tt$N_Early,
                     factor2 = tt$N_Pred,
                     factor3 = tt$N_AgeFive,
                     factor4 = tt$N_AgeThree, 
                     factor5 = tt$N_E_P_Interact, 
                     factor6 = tt$N_Early_AgeFive_Interact, 
                     factor6a = tt$N_Early_AgeThree_Interact, 
                     factor7 = tt$N_Pred_AgeFive_Interact, 
                     factor7a = tt$N_Pred_AgeThree_Interact, 
                     factor8 = tt$N_Early_Pred_AgeFive_Interact, 
                     factor8a = tt$N_Early_Pred_AgeThree_Interact, 
                     N = nrow(tt), J = nlevels(as.factor(tt$Subject)), 
                     Subj = as.integer(as.factor(tt$Subject)),
                     K = nlevels(as.factor(tt$CharacterLength.ms)),
                     Item = as.integer(as.factor(tt$CharacterLength.ms)),
                     sdscal = sd(residuals(lm(rt ~ Pred*Early.Late, tt))))

initf1 <- function() {
  list(beta = c(1,rep(0,10)), beta_t = c(1,rep(0,10)),beta_s = c(1,rep(0,10)))
}

eg_expt2_full <- stan(file="fixEf_SimpleFullAge_expt2_beta.stan",
                      data=stanDat_full,
                      chains = 4, iter = 3000, init = initf1, control = list(adapt_delta = 0.9))

save(eg_expt2_full_age, file = "/exports/eddie/scratch/hrabagli/expt2_full.RDATA")
