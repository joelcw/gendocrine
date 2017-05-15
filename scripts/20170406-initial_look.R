library(lme4)

df <- read.delim("fake-numbers.txt",header=TRUE,as.is = TRUE)
# head(df)
# unique(df$subseg)

# only look at the preaspiration subsegmental features
  # pre = preaspiration
  # cr = creak?
  # br = breathiness?
preasp <- df[df$subseg=="pre",]
# head(preasp)
# length(unique(preasp$V)) # how many vowels are in this dataset?

# What do we want the LMER to look like, ideally?
# mdl <- lmer(preasp_len ~ ratio+age + V + C1 + foll_context_immediate + foot + stress + foll_word +
#              (1 + ratio+age + V + C1 + foll_context_immediate + foot + stress + foll_word|filename), data=preasp, REML=FALSE)
# # there might not be enough data to include foll_word (rank deficient model, maybe)


# this model is just to test what is possible with the structure of this dataset, even if the numbers are noise/wrong
mdl <- lmer(sub_dur_FINAL ~ V + C1 + foll_context_immediate + foot +
              (1|filename), data=preasp, REML=FALSE)
summary(mdl)

# is the data in this model normally distributed? yes = points lie along the line
qqplot(residuals(mdl),fitted(mdl))
qqnorm(residuals(mdl));qqline(residuals(mdl))

# what happens if we include the interaction of Vowel and Consonant (consonant that triggers preaspiration)
mdl.int <- lmer(sub_dur_FINAL ~ V + C1 + V:C1 + foll_context_immediate + foot +
                  (1|filename), data=preasp, REML=FALSE)
anova(mdl,mdl.int)

# How much does the following consonant actually contribute to the model?
mdl.C <- lmer(sub_dur_FINAL ~ V + foll_context_immediate + foot +
                (1|filename), data=preasp, REML=FALSE)
anova(mdl.C,mdl)

