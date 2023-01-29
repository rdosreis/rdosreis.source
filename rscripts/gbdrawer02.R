
library(readr)

gbd_copd <- read_csv(here::here("data",
                                "IHME-GBD_2019_DATA-6d31d6bf-1.csv"))

gbd_copd <- subset(x = gbd_copd, age != "<28 days")

gbd_copd$age <- factor(x = gbd_copd$age,
                       levels = c(unique(gbd_copd$age)[22:23], unique(gbd_copd$age)[1:21]),
                       labels = c(c("Early Neonatal", "Late Neonatal", "Post Neonatal"),
                                  unique(gbd_copd$age)[2:17],
                                  paste(unique(gbd_copd$age)[18:20], "years"),
                                  unique(gbd_copd$age)[21])
                       )

gbd_copd$sex <- factor(x = gbd_copd$sex)
gbd_copd$sex <- relevel(gbd_copd$sex, ref = "Male")

gbd_copd$measure <- factor(x = gbd_copd$measure,
                           labels = c("YLDs", "YLLs"))
gbd_copd$measure <- relevel(gbd_copd$measure, ref = "YLLs")

gbd_copd$sex_measure <- interaction(gbd_copd$sex, gbd_copd$measure, sep = ", ")

library(ggplot2)
library(RColorBrewer)
library(wesanderson)

p1 <- ggplot(data = gbd_copd,
             mapping = aes(x = age, y = val, fill = sex_measure)) +
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_manual(values = rev(wes_palette(n = 4, name = "GrandBudapest2"))) +
  scale_y_continuous(breaks = c(0, 5000000, 10000000)) +
  labs(x = "Age group", y = "DALYs", fill = "") +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(hjust = 1, angle = 45, size = 8))
p1

p2 <- ggplot(data = gbd_copd,
             mapping = aes(x = age, y = val, fill = measure)) +
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_manual(values = rev(wes_palette(n = 2, name = "GrandBudapest2"))) +
  # scale_y_continuous(breaks = c(0, 5000000, 10000000)) +
  labs(x = "Age group", y = "DALYs", fill = "") +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(hjust = 1, angle = 45, size = 8)) +
  facet_grid(~sex)
p2

# Piramide?

library(plyr)

p3 <- ggplot(data = gbd_copd,
             mapping = aes(x = age, y = val, fill = measure)) +
  geom_bar(stat = "identity", width = 0.6, subset=.(sex == "Female")) +
  geom_bar(stat = "identity", width = 0.6, subset=.(sex == "Male"), aes(y=..count..*(-1))) +
  # scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip() +
  scale_fill_manual(values = rev(wes_palette(n = 2, name = "GrandBudapest2"))) +
  # scale_y_continuous(breaks = c(0, 5000000, 10000000)) +
  labs(x = "Age group", y = "DALYs", fill = "") +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(hjust = 1, angle = 45, size = 8))
  
p3
