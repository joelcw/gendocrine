library(devtools)
install_github("jofrhwld/UhUm")
library(UhUm)
library(dplyr)
library(ggplot2)

um <- um_PNC %>%
  mutate(word = gsub("_AND","", word))%>%
  filter(word %in% c("UH","UM"))


um %>%
  mutate(is_um = (word == "UM") * 1) %>%
  group_by(idstring, age, year,sex)%>%
  summarise(um = mean(is_um))%>%
  mutate(dob = year-age)->speaker_means


speaker_means %>%
  mutate(decade = floor(dob/10)*10)%>%
  group_by(sex, decade)%>%
  summarise(um = mean(um))->decade_means



ggplot(speaker_means, aes(dob, um, color = sex))+
  geom_point(size = 1)+
  geom_point(data = decade_means, aes(decade, um), size = 3)+
  geom_line(data = decade_means, aes(decade, um), size = 1)+
  scale_color_brewer("gender", palette = "Dark2")+
  xlab("date of birth")+
  ylab("proportion 'UM' of all filled pauses")+
  theme_bw()->p

ggsave(p, file = "../figures/um.pdf", width = 8/1.5, height = 5/1.5)
