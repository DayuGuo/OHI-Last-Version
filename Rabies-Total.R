library(readxl)
library(readr)

library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

GBD <- read_excel("D.5.4.1_raw.xlsx")

dftest <- read_csv("total_0228_last Version.csv")

data = left_join(dftest,GBD, by = "iso")

write.csv(data,file = "Rabies-OHI.csv")


test <- data %>%
  mutate(
    region = case_when(
      region == "Middle East and North Africa" ~ "Middle East and North Africa",
      region == "Europe and Central Asia" ~ "Europe and Central Asia",
      region == "East Asia and Pacific" ~ "East Asia and Pacific",
      region == "Latin America and The Caribbean" ~ "Latin America and The Caribbean",
      region == "North America" ~ "North America",
      region == "South Asia" ~ "South Asia",
      region == "Sub-Saharan Arica" ~ "Sub-Saharan Arica",
      TRUE ~ region # All the other remain the same
    )
  )

country_highlight <- c(
  "Germany", "Norway", "United States of America", "Greece", "Singapore", "Rwanda", 
  "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", 
  "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )


expohi = ggplot(test, aes(GBD, total)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = region, fill = region),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 10/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  xlab("Rabies DALYS") +
  ylab("Global One Health Intrinsic Drivers Index scores")+
  theme_test() 

ggsave("OHI-Rabies-region.pdf", width=8, height=4, dpi=600)

