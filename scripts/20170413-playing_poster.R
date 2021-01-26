library(ggplot2); library(lme4)
data <- read.delim("20170411-results_poster.txt", header=TRUE, as.is=TRUE)
  data$proportion <- data$duration/data$wordDur
  data$logProp <- log(data$proportion)

head(data)

dataSubseg <- data[data$subseg=="pre"|data$subseg=="br"|data$subseg=="cr",]


preasp <- data[data$subseg=="pre",]


plot(preasp$logProp~preasp$logDigit)
abline(lm(preasp$logProp~preasp$logDigit))

cor.test(preasp$logProp,preasp$logDigit)
cor.test(preasp$duration,preasp$age)

mdl.test <- lmer(logProp ~ logDigit+ethnicity2 + logDigit:ethnicity2 +age + interviewer + V + C1 +stress+foot+foll_interval+ (1|speaker), data=preasp, REML=FALSE)
summary(mdl.test)
print(summary(mdl.test),correlation=TRUE)

mdl.test.int <- lmer(logProp ~ logDigit+ethnicity2 + age + interviewer + V + C1 +stress+foot+foll_interval+ (1|speaker), data=preasp, REML=FALSE)
mdl.test.dig <- lmer(logProp ~ ethnicity2 + age + interviewer + V + C1 +stress+foot+foll_interval+ (1|speaker), data=preasp, REML=FALSE)
mdl.test.age <- lmer(logProp ~ logDigit+ethnicity2 + logDigit:ethnicity2 + interviewer + V + C1 +stress+foot+foll_interval+ (1|speaker), data=preasp, REML=FALSE)

anova(mdl.test,mdl.test.int)
anova(mdl.test.int,mdl.test.dig)
anova(mdl.test,mdl.test.age)

anova(mdl.test,mdl.test.dig)

p<- ggplot(preasp, aes(x=logDigit,y=logProp)) + geom_point(color="#00A9FF",alpha=.5) + geom_smooth(method="lm") + # ,shape=tomboyYorN,color=speaker # [complete.cases(preasp$tomboyYorN),]
ggtitle("") + xlab("Log of 2D:4D ratio")+ylab("Log of preaspiration duration as proportion of word duration") + theme_bw() + theme(panel.border = element_blank())
#ggsave(p, file="plotXYZ.pdf", width=8,height=5)

scales::show_col(scales::hue_pal()(16))
