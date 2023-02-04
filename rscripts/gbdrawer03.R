library(readr)

gbd_copd <- read_csv(file = here::here("data",
                                       "IHME-GBD_2019_DATA-4fea7ca1-1.csv"))

gbd_copd$age <- factor(
  x = gbd_copd$age,
  levels = unique(gbd_copd$age),
  labels = c(
    c("Early Neonatal", "Late Neonatal", "Post Neonatal"),
    unique(gbd_copd$age)[4:19],
    paste(unique(gbd_copd$age)[20:22], "years"),
    unique(gbd_copd$age)[23]
  )
)

gbd_copd$val[gbd_copd$sex == "Male"] <- -gbd_copd$val[gbd_copd$sex == "Male"]

library(dplyr)
library(ggplot2)
library(RColorBrewer)

gbd_copd_19 <- gbd_copd %>% 
  filter (year == 2019)

p <- ggplot(data = gbd_copd_19,
            mapping = aes(x = age,
                          y = val,
                          fill = sex)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = abs,
                     limits = 
                       max(max(gbd_copd_19$val, na.rm = TRUE),
                           abs(min(gbd_copd_19$val))) * c(-1,1)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age group", y = "DALYs", fill = "Sex",
       title = "Carga global de COPD. DALYs por faixa etária, ano: 2019",
       caption = "Global Burden of Disease Study 2019 (GBD 2019) Results. 
       Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. 
       Available from https://vizhub.healthdata.org/gbd-results/.") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()
p

p <- ggplot(data = gbd_copd,
            mapping = aes(x = age,
                          y = val,
                          fill = sex)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = abs,
                     limits = 
                       max(max(gbd_copd_19$val, na.rm = TRUE),
                           abs(min(gbd_copd_19$val))) * c(-1,1)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age group", y = "DALYs", fill = "Sex",
       title = "Carga global de COPD. DALYs por faixa etária",
       caption = "Global Burden of Disease Study 2019 (GBD 2019) Results. 
       Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. 
       Available from https://vizhub.healthdata.org/gbd-results/.") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() +
  facet_wrap( ~ year)
p

library(gganimate)

gbd_copd$year <- integer(gbd_copd$year)

p <- ggplot(data = gbd_copd,
            mapping = aes(x = age,
                          y = val,
                          fill = sex)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = abs,
                     limits = 
                       max(max(gbd_copd$val, na.rm = TRUE),
                           abs(min(gbd_copd$val))) * c(-1,1)) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age group", y = "DALYs", fill = "Sex",
       title = "Carga global de COPD. DALYs por faixa etária, ano: {as.integer(frame_time)}",
       caption = "Global Burden of Disease Study 2019 (GBD 2019) Results. 
       Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020. 
       Available from https://vizhub.healthdata.org/gbd-results/.") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() +
  transition_time(year) +
  ease_aes('linear')
p

# gganimate(p, filename = "images/daly-copd.gif",
          # ani.width = 1000, ani.height = 1600, ani.res = 200)
