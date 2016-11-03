library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)


tomboys <- read.csv("gendocrine/datasets/BethTomboyDataset.csv")


#Recoding tomboy column in a standard way for binary variables

tomboys$yesOrNo <- ifelse(tomboys$Tomboy == 2, 0,1)
tomboys$yesOrNo <- as.numeric(as.character(tomboys$yesOrNo))


#Zing the age predictor just in case

tomboys$zAge <- scale(tomboys$Age, center=TRUE, scale=TRUE)

#log-ing the finger ratio; I use natural log here

tomboys$lnRight <- log(tomboys$rightHand)

tomboys$lnLeft <- log(tomboys$LeftHand)

#writing dataset to file

write.csv(tomboys, file="~/gendocrine/datasets/tomboys.csv", row.names = FALSE)

#logistic model

tomboys.fit <- glm(yesOrNo~lnRight*zAge, family = binomial, data=tomboys)
summary(tomboys.fit)
anova(tomboys.fit, test = "Chisq")

#taking Age out, as it doesnt seem to do anything 

tomboys.fit2 <- glm(yesOrNo~lnRight, family = binomial, data=tomboys)
summary(tomboys.fit2)
anova(tomboys.fit2, test = "Chisq")

#same with left hand 

tomboys.fit3 <- glm(yesOrNo~lnLeft, family = binomial, data=tomboys)
summary(tomboys.fit3)
anova(tomboys.fit3, test = "Chisq")