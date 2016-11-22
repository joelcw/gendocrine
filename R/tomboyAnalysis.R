library(ggplot2)
library(plyr)
library(dplyr)
library(lme4)
library(RColorBrewer)
library(gam)

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


#plot, unbinned data, with logistic and gam

tomboys$test <- tomboys$yesOrNo

p <- ggplot(tomboys, aes(lnRight, yesOrNo)) + scale_y_continuous(name = "Probability: Tomboy Or Not?", breaks=seq(0,1,by=0.1), labels=c("Not Tomboy",seq(0.1,0.9,by = 0.1),"Tomboy") ) + scale_x_continuous(name = "\nln(right hand 2D:4D ratio)")  + geom_point() + scale_alpha_continuous(guide="none", limits = c(0,.9))  + theme_bw() + theme(panel.border = element_blank()) + geom_jitter(height = 0.1) + geom_smooth(alpha = 0.2, method="glm",method.args = list(family = "binomial"),fullrange=T, colour="purple")

ggsave(p, file = "~/gendocrine/figures/tomboysPurple.pdf", width = 8, height = 5)

#fuckin around with splines

library(splines)
library(MASS)

ggplot(tomboys, aes(lnRight, test)) + scale_y_continuous(name = "Probability: Tomboy Or Not?", breaks=seq(0,1,by=0.1), labels=c("Not Tomboy",seq(0.1,0.9,by = 0.1),"Tomboy") ) + scale_x_continuous(name = "\nln(right hand 2D:4D ratio)")  + geom_smooth(alpha = 0.2, method="gam",formula = y ~ ns(x,3),fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.9)) + scale_color_brewer(palette = "Dark2") + theme_bw() + theme(panel.border = element_blank()) + geom_point() 


#Trying my first boxplot

tomboys$id <- ifelse(tomboys$yesOrNo == 1, "Tomboy","Not Tomboy")

p <- ggplot(tomboys, aes(id,lnRight, group=id)) + scale_y_continuous(name = "ln(right hand 2D:4D ratio)") + scale_x_discrete(name = "\nParticipant Group") + geom_point() + geom_boxplot(fill=c("purple","green")) + theme_bw() + theme(panel.border = element_blank()) + geom_jitter(width = 0.3)

ggsave(p, file = "~/gendocrine/figures/tomboysBoxplot.pdf", width = 8, height = 5)

