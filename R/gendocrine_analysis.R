#' ---
#' title: "Gendocrine Analysis"
#' author: "Josef Fruehwald"
#' date: "August 27, 2015"
#' output: html_document
#' ---
  

#' # Load Libraries
library(dplyr)
library(reshape2)
library(magrittr)
library(lme4)
library(ggplot2)
library(scales)
library(knitr)
library(boot)

library(beepr)

#' # Load and prep data

dat <- read.csv("../datasets/annie.csv")

dat2 <- read.csv("../datasets/somemore.csv", header = F)

colnames(dat2) <- c("Speaker","Filler", "PreSeg", "FolSeg", "Age","Foo","Finger","Interviewer", "Words")

dat2 %<>%
  mutate(fol_sil = (FolSeg == "P")*1,
         pre_sil = (PreSeg == "P")*1,
         Speaker = as.character(Speaker),
         is_um = (Filler == "um")*1,
         log.finger = log(Finger, base = 1.2))

dat %<>%
  mutate(Speaker = as.character(Speaker),
         is_um = (Filler == "um")*1,
         pre_sil = (Preceding == "P")*1,
         fol_sil = (Following == "P")*1,
         gender0 = Gender - 3,
         log.finger = log(Finger, base = 1.2))


dat %>% 
  rbind_list(dat2)%>%
  mutate(age_c = (Age-20))%>%
  filter(Age < 30) ->dat3


#' # Modelling

#' Fitting 3 models. The random effects structure is the maximal one that will converge. 
#' The only possible way t
#' 
#' - pre_sil and fol_sil included in all models as a known important covariate (Clark and Fox Tree 2002)
#' - age_c is included since this is a change in progress, but wasn't actually significant 
#' - stepwise comparison to see how log.finger ought to be entered with respect to age
mod <- glmer(is_um ~  pre_sil * fol_sil + age_c * log.finger + (1|Speaker), data = dat3, family = binomial)
mod2 <- glmer(is_um ~ pre_sil*fol_sil + age_c + log.finger + (1|Speaker), data = dat3, family = binomial)
mod3 <- glmer(is_um ~ pre_sil*fol_sil + age_c + (1|Speaker), data = dat3, family = binomial)

anova(mod, mod2, mod3)%>%kable()

#' Maximal interaction not worth it. Going with `age + log.finger`, mod2.


#' # Effect Size Reliability Estimation

#' ## Step 1: Bootstrap replication.
fixef <- bootMer(mod2, FUN = fixef, nsim = 1000, verbose = T)

#' ### Visualizing 

log.finger_dens <- density(fixef$t[,"log.finger"])

density_fit <- data.frame(x = log.finger_dens$x,
                          y = log.finger_dens$y)

ci <- boot.ci(fixef, type = "perc", index = "log.finger")$percent[4:5]

#' Confidence interval = `r ci`

ggplot(density_fit, aes(x, y)) + 
  geom_ribbon(aes(ymin = 0, ymax = y),
              color = "black", fill = NA)+
  geom_ribbon(data = density_fit %>% filter(x > ci[1], x < ci[2]),
              aes(ymin = 0, ymax = y),
              fill = "grey40")+
  geom_vline(x = 0, linetype = 2)+
  geom_vline(x = fixef(mod2)["log.finger"])+
  xlab("log.finger")+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())


#' Visualize effect
#' 
inverse_logit <- function(x){exp(x)/(exp(x)+1)}

pred_data <- data.frame(pre_sil = 0.5, fol_sil = 0.5,
                        age_c = 0, log.finger = seq(-0.41,1, length = 100),
                        index = 1:100)

pred_mat <- model.matrix(~pre_sil*fol_sil + age_c + log.finger, data = pred_data)

pred_data$fit <- (pred_mat %*% fixef(mod2))[,1]
boot_fits <- pred_mat %*% t(fixef$t)
boot_fits_df <- melt(boot_fits)
colnames(boot_fits_df)[1] <- "index"

boot_fits2<- left_join(boot_fits_df, pred_data)


dat3 %>%
  group_by(Speaker, log.finger)%>%
  summarise(um = mean(is_um))->speaker_means

ggplot(speaker_means, aes(log.finger, um)) + 
  geom_line(data = boot_fits2, aes(y = inverse_logit(value), group = Var2), alpha = 0.07)+
  geom_line(data = pred_data, aes(y = inverse_logit(fit)), color = "red")+
  geom_point(size = 4, shape = 21, color = "black", fill = "#1b9e77")+
  ylim(0.5,1)+
  theme_bw()->p

ggsave(p, file = "../figures/finger_effect.pdf", width = 8/1.25, height = 5/1.25)


#' ## Step 2: Leave one out

speakers <- unique(dat3$Speaker)

cross_fixef <- matrix(nrow = length(speakers), ncol = length(fixef(mod2)))

for(i in seq(along = speakers)){
  less_dat <- filter(dat3, Speaker != speakers[i])
  less_mod <- glmer(is_um ~ pre_sil*fol_sil + age_c + log.finger + (1|Speaker), 
                    data = less_dat, family = binomial)
  cross_fixef[i,] <- fixef(less_mod)
}

influence_df <- data.frame(Speaker = speakers,
                           influence = fixef(mod2)[5] - cross_fixef[,5])

speaker_means2 <- speaker_means %>% left_join(influence_df)

ggplot(speaker_means2, aes(log.finger, um)) + 
  geom_line(data = boot_fits2, aes(y = inverse_logit(value), group = Var2), alpha = 0.07)+
  geom_line(data = pred_data, aes(y = inverse_logit(fit)), color = "red")+
  geom_point(size = 4, aes(color = influence))+
  ylim(0.5,1)+
  scale_color_gradient2(mid = "#1b9e77", high = muted("red"), low = muted("blue"))+
  theme_bw()


data_frame(log.finger = cross_fixef[,5]) %>%
  ggplot(aes(log.finger))+
    geom_bar()+
    geom_vline(x = fixef(mod2)[5], color = "red")+
    theme_bw()


#' ## Step 3: Permutation Test
#' 
#' first, get by-speaker data to permute

speaker_dat <- dat3 %>% group_by(Speaker, log.finger) %>% tally()


#' Functions to permute the log.finger variable and fit a model

one_permute <- function(speaker_dat, dat, indicies){
  sp <- speaker_dat
  sp$new.finger = speaker_dat$log.finger[indicies]
  sp <- sp[,c("Speaker","new.finger")]
  out<- left_join(sp, dat)
  return(out)
}

fit_permute <- function(permute_dat){
  mod <- glmer(is_um ~ pre_sil*fol_sil + age_c + new.finger + (1|Speaker), 
               data = permute_dat, family = binomial)
  return(fixef(mod))
}

#' Pre-allocate space for the fixed effects

permute_coefs <- matrix(nrow = 10000, ncol = length(fixef(mod2)))

#' Fit a model for a randomly permuted log.finger variable
for(i in 7988:10000){
  permute_coefs[i,] <- one_permute(speaker_dat, dat3, sample(1:14)) %>% fit_permute()
};beep(8)


prop.table(table(abs(permute_coefs[,5] > fixef(mod2)[5])))
