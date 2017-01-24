library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)
library(RColorBrewer)
library(gam)



tomboys <- read.csv("gendocrine/datasets/tomboys.csv", strip.white = TRUE)




#Recoding tomboy column in a standard way for binary variables
#tomboys$yesOrNo <- ifelse(tomboys$Tomboy == 2, 0,1)
#tomboys$yesOrNo <- as.numeric(as.character(tomboys$yesOrNo))

#Note that the various extra columns added below, zAge, lnRight, lnLeft, are already in the dataset above, but I've included these commands so that you can see what I've done.

#Zing the age predictor just in case

tomboys$zAge <- scale(tomboys$Age, center=TRUE, scale=TRUE)


#log-ing the finger ratio; I use natural log here

tomboys$lnRight <- log(tomboys$rightHand)

tomboys$lnLeft <- log(tomboys$LeftHand)

#writing dataset to file

#write.csv(tomboys, file="~/gendocrine/datasets/tomboys.csv", row.names = FALSE)

#logistic model

tomboys.fit <- glm(yesOrNo~lnRight*zAge, family = binomial, data=tomboys)
summary(tomboys.fit)
anova(tomboys.fit, test = "Chisq")



#model without Age 

tomboys.fit2 <- glm(yesOrNo~lnRight, family = binomial, data=tomboys)
summary(tomboys.fit2)
anova(tomboys.fit2, test = "Chisq")

#model with main effect of Age, but no interaction

tomboys.fit4 <- glm(yesOrNo~lnRight+Age, family = binomial, data=tomboys)
summary(tomboys.fit4)



#AIC and BIC for model comparison: main effect of Age, and interaction with Age

AIC(tomboys.fit2) - AIC(tomboys.fit)
BIC(tomboys.fit2) - BIC(tomboys.fit)

AIC(tomboys.fit2) - AIC(tomboys.fit4)
BIC(tomboys.fit2) - BIC(tomboys.fit4)




#Mann-Whitney-Wilcoxon U Test

wilcox.test(lnRight~yesOrNo, data=tomboys)


#same logistic model with left hand 

tomboys.fit3 <- glm(yesOrNo~lnLeft, family = binomial, data=tomboys)
summary(tomboys.fit3)
anova(tomboys.fit3, test = "Chisq")




#plot, unbinned data, with logistic and gam

tomboys$test <- tomboys$yesOrNo

p <- ggplot(tomboys, aes(lnRight, yesOrNo)) + scale_y_continuous(name = "Probability: Tomboy Or Not?", breaks=seq(0,1,by=0.1), labels=c("Not Tomboy",seq(0.1,0.9,by = 0.1),"Tomboy") ) + scale_x_continuous(name = "\nln(right hand 2D:4D ratio)")  + geom_point() + scale_alpha_continuous(guide="none", limits = c(0,.9))  + theme_bw() + theme(panel.border = element_blank()) + geom_jitter(height = 0.1) + geom_smooth(alpha = 0.2, method="glm",method.args = list(family = "binomial"),fullrange=T, colour="red")

ggsave(p, file = "~/gendocrine/figures/tomboysUnbinnedLogisticRed.ps", width = 8, height = 5)

#fuckin around with splines

library(splines)
library(MASS)

ggplot(tomboys, aes(lnRight, test)) + scale_y_continuous(name = "Probability: Tomboy Or Not?", breaks=seq(0,1,by=0.1), labels=c("Not Tomboy",seq(0.1,0.9,by = 0.1),"Tomboy") ) + scale_x_continuous(name = "\nln(right hand 2D:4D ratio)")  + geom_smooth(alpha = 0.2, method="gam",formula = y ~ ns(x,3),fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.9)) + scale_color_brewer(palette = "Dark2") + theme_bw() + theme(panel.border = element_blank()) + geom_point() 


#Trying my first boxplot

tomboys$id <- ifelse(tomboys$yesOrNo == 1, "Tomboy","Not Tomboy")

p <- ggplot(tomboys, aes(id,lnRight, group=id)) + scale_y_continuous(name = "ln(right hand 2D:4D ratio)") + scale_x_discrete(name = "\nParticipant Group") + geom_point() + geom_boxplot(fill=c("purple","green")) + theme_bw() + theme(panel.border = element_blank()) + geom_jitter(width = 0.3)

ggsave(p, file = "~/gendocrine/figures/tomboysBoxplot.ps", width = 8, height = 5)




#tomboys <- read.csv("CurrentLx/gender/tomboyArticleStuff/tomboysWithEthnicity.csv", strip.white=TRUE)
#colnames(tomboys)[20] <- "Ethnicity"

#Analysis including ethnicity

#model without Age, without Ethnicity 

tomboysNoEth.fit <- glm(yesOrNo~lnRight, family = binomial, data=tomboys)
summary(tomboysNoEth.fit)
anova(tomboysNoEth.fit, test = "Chisq")

#above model with Ethnicity

tomboysEth.fit <- glm(yesOrNo~lnRight*Ethnicity, family = binomial, data=tomboys)
summary(tomboysEth.fit)
anova(tomboysEth.fit, test = "Chisq")

AIC(tomboysNoEth.fit) - AIC(tomboysEth.fit)
BIC(tomboysNoEth.fit) - BIC(tomboysEth.fit)


tomboysEth2.fit <- glm(yesOrNo~lnRight+Ethnicity, family = binomial, data=tomboys)
summary(tomboysEth2.fit)
anova(tomboysEth2.fit, test = "Chisq")

AIC(tomboysNoEth.fit) - AIC(tomboysEth2.fit)
BIC(tomboysNoEth.fit) - BIC(tomboysEth2.fit)


#Trying with whites only

tomboysWhites <- subset(tomboys, Ethnicity == "white")

library(gdata)

tomboysWhites <- droplevels(tomboysWhites)

tomboysWhites.fit <- glm(yesOrNo~lnRight, family = binomial, data=tomboysWhites)
summary(tomboysWhites.fit)
anova(tomboysWhites.fit, test = "Chisq")


#Adding men to the box plot:

men <- read.csv("CurrentLx/gender/tomboyArticleStuff/men.csv", strip.white=TRUE)

colnames(men)[1] <- "Participant"
colnames(men)[6] <- "rightHand"
colnames(men)[8] <- "LeftHand"

men$lnRight <- log(men$rightHand)

men$lnLeft <- log(men$LeftHand)

plottomboys <- data.frame(tomboys$Participant,tomboys$lnRight,tomboys$id, stringsAsFactors=FALSE, check.names=TRUE)
colnames(plottomboys) <- c("Participant","lnRight","id")

men$id <- "Men"

plotmen <- data.frame(men$Participant,men$lnRight,men$id, stringsAsFactors=FALSE, check.names=TRUE)
colnames(plotmen) <- c("Participant","lnRight","id")

plotboys <- rbind(plottomboys,plotmen)
  

p <- ggplot(plotboys, aes(id,lnRight, group=id)) + scale_y_continuous(name = "ln(right hand 2D:4D ratio)") + scale_x_discrete(name = "\nParticipant Group") + geom_point() + geom_boxplot(fill=c("purple","green","yellow")) + theme_bw() + theme(panel.border = element_blank()) + geom_jitter(width = 0.3)

ggsave(p, file = "~/gendocrine/figures/tomboysMenBoxplot.ps", width = 8, height = 5)

#Getting an effect size for men

plotboys$Sex <- ifelse(plotboys$id == "Men", "Men","Women")

tomboysMen.fit <- glm(Sex~lnRight, family = binomial, data=plotboys)
summary(tomboysMen.fit)
anova(tomboysMen.fit, test = "Chisq")


#Mann-Whitney-Wilcoxon U Test for men vs. women

wilcox.test(lnRight~Sex, data=plotboys)

write.csv(plotboys, file="~/gendocrine/datasets/tomboysWithMen.csv", row.names = FALSE)