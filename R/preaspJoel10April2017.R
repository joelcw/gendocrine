library(lme4)

df <- read.delim("~/CurrentLx/gender/preaspirationStuff/2017April11-results_poster.txt",header=TRUE,sep="\t")


#demographic and ratio data

#ratio.df <- read.csv("~/gendocrine/datasets/ratioDemogApril2017.csv", header=T)

#merging the two data frames on speaker col

#fulldata.df <- merge(df, ratio.df, by=c("speaker"), all.y=FALSE)


#collapsing non-White ethnicities together, sorry Universe for my iniquity

#fulldata.df$ethnicity2 <- ifelse(fulldata.df$ethnicity != "White", "notWhite","White")

#logging the percentage of vowel in "pre", "br", and "cr" into new columns for analysis

#fulldata.df$logPre <- log(fulldata.df$pre)
#fulldata.df$logBr <- log(fulldata.df$br)
#fulldata.df$logCr <- log(fulldata.df$cr)

fulldata.df$logDur <- log(fulldata.df$duration)

#logging the digit ratio

#fulldata.df$logDigit <- log(fulldata.df$digitRatio)

#writing merged data frame to my github directory

fulldata.df <- df

write.csv(fulldata.df, file="~/gendocrine/datasets/fullPreAspData10April2017.csv", row.names = FALSE)

# this model is just to test what is possible with the structure of this dataset, even if there are problems

#Note: including V*C1 creates a rank-deficient model 

#this model uses raw duration, not duration as a proportion of the vowel. It also uses all subsegments, treating their identity as a variable.

simple.fit <- lmer(duration ~ V + C1 + subseg + logDigit + digitMethod + ethnicity2 + age + interviewer + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=fulldata.df, REML=FALSE)
summary(simple.fit)


# is the data in this model normally distributed? yes = points lie along the line
qqplot(residuals(simple.fit),fitted(simple.fit))
qqnorm(residuals(simple.fit));qqline(residuals(simple.fit))


noAge.fit <- lmer(duration ~ V + subseg + C1 + logDigit + digitMethod + ethnicity2 + interviewer + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + (1|speaker), data=fulldata.df, REML=FALSE)

summary(noAge.fit)
anova(noAge.fit, simple.fit)

noAgeNoDig.fit <- lmer(duration ~ V + C1 + subseg + digitMethod + ethnicity2 + interviewer + (1|speaker), data=fulldata.df, REML=FALSE)
anova(noAge.fit, noAgeNoDig.fit)


#preaspiration durations alone
qqplot(fulldata.df[fulldata.df$subseg=="pre",]$logDigit,fulldata.df[fulldata.df$subseg=="pre",]$duration)

#creating a column for subseg normalized by word duration: log(subseg-dur / word-dur)

fulldata.df$logDur <- log(fulldata.df$duration/fulldata.df$wordDur)


#write new version of data set

write.csv(fulldata.df, file="~/gendocrine/datasets/fullPreAspData10April2017.csv", row.names = FALSE)

#Subsetting because there are some weird annotations in subseg, like "brCOULDbePREaBIT"

fulldata1.df <- subset(fulldata.df,subseg == "pre" | subseg == "br" | subseg == "cr")

library(gdata)

fulldata1.df <- droplevels(fulldata1.df)

#using logDur and all subsegs

simpleLog.fit <- lmer(logDur ~ V + C1 + subseg + logDigit + digitMethod + ethnicity2 + age + interviewer + stress + foot + foll_interval + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=fulldata1.df, REML=FALSE)
summary(simpleLog.fit)

LogDurNoDigit.fit <- lmer(logDur ~ V + C1 + subseg + age + digitMethod + ethnicity2 + interviewer + stress + foot + foll_interval + (1|speaker), data=fulldata1.df, REML=FALSE)

summary(LogDurNoDigit.fit)
anova(LogDurNoDigit.fit,simpleLog.fit)

#subsetting to preaspiration only

preasp.df <- subset(fulldata.df,subseg == "pre")

library(gdata)

preasp.df <- droplevels(preasp.df)


#using logDur and just preasp

LogPreasp.fit <- lmer(logDur ~ V + C1 + logDigit + digitMethod + ethnicity2 + age + interviewer + stress + foot + foll_interval + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=preasp.df, REML=FALSE)
summary(LogPreasp.fit)

LogPreaspNoDigit.fit <- lmer(logDur ~ V + C1 + age + digitMethod + ethnicity2 + interviewer + stress + foot + foll_interval + (1|speaker), data=preasp.df, REML=FALSE)

summary(LogPreaspNoDigit.fit)
anova(LogPreaspNoDigit.fit,LogPreasp.fit)


#trying everything besides preasp...maybe there's something wrong here.
#subsetting to preaspiration only

nonPreasp.df <- subset(fulldata.df,subseg == "br" | subseg == "cr")

library(gdata)

nonPreasp.df <- droplevels(nonPreasp.df)

LogNonPre.fit <- lmer(logDur ~ V + C1 + logDigit + digitMethod + ethnicity2 + age + interviewer + stress + foot + foll_interval + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=nonPreasp.df, REML=FALSE)
summary(LogNonPre.fit)

LogNonPreNoDigit.fit <- lmer(logDur ~ V + C1 + age + digitMethod + ethnicity2 + interviewer + stress + foot + foll_interval + (1|speaker), data=nonPreasp.df, REML=FALSE)

summary(LogNonPreNoDigit.fit)
anova(LogNonPreNoDigit.fit,LogNonPre.fit)


#Try preasp and br together...maybe I should try each separately?? except cr must be dif ??

preBr.df <- subset(fulldata.df,subseg == "br" | subseg == "pre")

library(gdata)

preBr.df <- droplevels(preBr.df)

LogPreBr.fit <- lmer(logDur ~ V + C1 + logDigit + digitMethod + ethnicity2 + age + interviewer + stress + foot + foll_interval + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=preBr.df, REML=FALSE)
summary(LogPreBr.fit)

LogPreBrNoDigit.fit <- lmer(logDur ~ V + C1 + age + digitMethod + ethnicity2 + interviewer + stress + foot + foll_interval + (1|speaker), data=preBr.df, REML=FALSE)

summary(LogPreBrNoDigit.fit)
anova(LogPreBrNoDigit.fit,LogPreBr.fit)

#Creak only

cr.df <- subset(fulldata.df,subseg == "cr")

library(gdata)

cr.df <- droplevels(cr.df)

LogCr.fit <- lmer(logDur ~ V + C1 + logDigit + digitMethod + ethnicity2 + age + interviewer + stress + foot + foll_interval + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + age*logDigit + (1|speaker), data=cr.df, REML=FALSE)
summary(LogCr.fit)

LogCrNoDigit.fit <- lmer(logDur ~ V + C1 + age + digitMethod + ethnicity2 + interviewer + stress + foot + foll_interval + (1|speaker), data=cr.df, REML=FALSE)

summary(LogCr.fit)
anova(LogCr.fit,LogCrNoDigit.fit)


#So, this is really strange, but age and digit ratio are highly correlated...maybe I'll leave each out of a model???
qqplot(fulldata.df$logDigit,fulldata.df$age)
qqplot(ratio.df$digitRatio,ratio.df$age)



#Residualizing age in ratio.df data frame, age~ratio

ratio.df$logDigit <- log(ratio.df$digitRatio)
age.fit <- lm(age~logDigit, data=ratio.df)
ratio.df$ageResid <- residuals(age.fit)

qqplot(ratio.df$age,ratio.df$logDigit)

#merging it back into the full data frame
age.df <- data.frame(ratio.df$speaker,ratio.df$ageResid)
colnames(age.df) <- c("speaker","ageResid")
fulldata2.df <- merge(fulldata1.df, age.df, by=c("speaker"), all.y=FALSE)



#Trying the preasp model again with residualized age

#subsetting to preaspiration only
preasp2.df <- subset(fulldata2.df,subseg == "pre")

preasp2.df <- droplevels(preasp2.df)


#using logDur and just preasp and residualized age
LogPreaspAgeResid.fit <- lmer(logDur ~ V + C1 + stress + foot + foll_interval + logDigit + digitMethod + ethnicity2 + ageResid + interviewer + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + (1|speaker), data=preasp2.df, REML=FALSE)
summary(LogPreaspAgeResid.fit)

LogPreaspNoDigitAgeResid.fit <- lmer(logDur ~ V + C1 + + stress + foot + foll_interval + ageResid + digitMethod + ethnicity2 + interviewer + (1|speaker), data=preasp2.df, REML=FALSE)

anova(LogPreaspNoDigitAgeResid.fit,LogPreaspAgeResid.fit)


#using ageResid with all subsegs

#using logDur and just preasp and residualized age
LogDurAgeResid.fit <- lmer(logDur ~ V + C1 + subseg + stress + foot + foll_interval + logDigit + digitMethod + ethnicity2 + ageResid + interviewer + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + (1|speaker), data=fulldata2.df, REML=FALSE)
summary(LogPreaspAgeResid.fit)

LogDurNoDigitAgeResid.fit <- lmer(logDur ~ V + C1 + subseg + stress + foot + foll_interval + ageResid + digitMethod + ethnicity2 + interviewer + (1|speaker), data=fulldata2.df, REML=FALSE)

anova(LogDurAgeResid.fit,LogDurNoDigitAgeResid.fit)



#Trying the preasp-only model again with NO AGE AT ALL

LogPreaspNoAge.fit <- lmer(logDur ~ V + C1 + stress + foot + foll_interval + logDigit + digitMethod + ethnicity2 + interviewer + logDigit*digitMethod + logDigit*ethnicity2 + logDigit*interviewer + (1|speaker), data=preasp2.df, REML=FALSE)
summary(LogPreaspNoAge.fit)

LogPreaspNoAgeNoDigit.fit <- lmer(logDur ~ V + C1 + stress + foot + foll_interval + digitMethod + ethnicity2 + interviewer + (1|speaker), data=preasp2.df, REML=FALSE)

anova(LogPreaspNoAgeNoDigit.fit,LogPreaspNoAge.fit)

#check age correl

#Subsetting ethnicity to make things simpler

preasp3.df <- subset(preasp2.df,ethnicity2 == "White")

preasp3.df <- droplevels(preasp3.df)

#Final model?? Took out interviewer*logDigit interaction...not sure if that's right:

LogPreaspNoEth.fit <- lmer(logDur ~ V + C1 + stress + foot + foll_interval + logDigit + digitMethod + age + interviewer + logDigit*digitMethod + age*logDigit + (1|speaker), data=preasp3.df, REML=FALSE)
summary(LogPreaspNoEth.fit)

LogPreaspNoEthNoDigit.fit <- lmer(logDur ~ V + C1 + stress + foot + foll_interval + age + digitMethod + interviewer + (1|speaker), data=preasp3.df, REML=FALSE)

summary(LogPreaspNoEthNoDigit.fit)
anova(LogPreaspNoEthNoDigit.fit,LogPreaspNoEth.fit)


#Plots

p <- ggplot(fulldata, aes(date, FormNum, color=Context, group=Context)) + scale_y_continuous(name = "Proportion Molten (vs. Melted)", breaks=seq(0,1,by=0.1), labels=c("Melted",seq(0.1,0.9,by = 0.1),"Molten") ) + scale_x_continuous(name = "\nYear") + stat_sum(aes(size=..n.., alpha=.2)) + scale_size_area(max_size=12) + stat_smooth(alpha = 0.2) + scale_alpha_continuous(guide="none", limits = c(0,.9)) + scale_color_brewer(palette = "Dark2") + theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/CurrentLx/OldNorse/gentdigs/FormByDateUnbinnedWithDots2.pdf", width = 8, height = 5)