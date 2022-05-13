library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)

library(readr)
library(esquisse)

ohi <- read_csv("total_0228.csv")
ohi$region = as.factor(ohi$region)

ohi$region = ordered(ohi$region,levels = c('Sub-Saharan Africa','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ohiseve = ggplot(ohi, aes(x=region, y=total, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Global One Health Index (GoHi) scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank(),text = element_text(size = 10))
        
        #axis.title = element_text(size = 14)

ggsave("test.tiff", units="in", width=8, height=4, dpi=600, compression = 'lzw')


### CDI

library(readr)
data <- read_csv("CDI 分布情况.csv")
library(ggridges)
library(ggplot2)

data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# Plot
cid=data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    text = element_text(size = 10)
  ) +
  xlab("Global One Health index (GOHI) scores") +
  ylab("Density")

ggsave("cditest.pdf", units="in", width=8, height=4, dpi=600)
