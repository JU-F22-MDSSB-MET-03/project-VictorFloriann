library(tidyverse)
library(scales)
library(glue)

#Visualization 1 : Overall temperature

# Visual 1
library(tidyverse)
library(scales)
library(glue)

t_data <- read_csv("GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()

annotation <- t_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall=1)

t_data %>%
  ggplot(aes(x=year, y=t_diff, fill=t_diff)) +
  geom_col(show.legend=FALSE) +
  geom_text(data = annotation, aes(x=x, label=year), color="black") +
  geom_text(x=1880, y=1, hjust=0,
            label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color="black") +
  scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="white"),
    legend.text = element_text(color="black")
  )
ggsave("figures/temperature_bar_plot.png", width=7, height=4)

#visual 2

library(tidyverse)

read_csv("GLB.Ts+dSST.csv", skip=1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na() %>%
  ggplot(aes(x=year, y=t_diff)) +
  geom_line(aes(color = "1"), size=0.5, show.legend = FALSE) + 
  geom_point(fill="white", aes(color = "1"), shape=21, show.legend=TRUE) +
  geom_smooth(se=FALSE, aes(color = "2"), size=0.5, span=0.15, show.legend = FALSE) +
  scale_x_continuous(breaks=seq(1880, 2023, 20), expand=c(0,0)) +
  scale_y_continuous(limits=c(-0.5, 1.5), expand=c(0,0)) +
  scale_color_manual(name=NULL,
                     breaks=c(1, 2),
                     values=c("gray", "black"),
                     labels=c("Annual mean", "Lowess smoothing"), 
                     guide = guide_legend(override.aes = list(shape=15, size=5))) +
  labs(
    x = "YEAR",
    y = "Temperature anomaly (C)",
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX"
    #subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS"
  ) +
  theme_light() +
  theme(
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b=10), color="red", face="bold"),
    plot.subtitle = element_text(size=8, margin = margin(b=10)),
    legend.position = c(0.15, 0.9),
    legend.title = element_text(size=0),
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(0,0,0,0)
  )

ggsave("figures/temperature_index_plot.png", width=6, height=4)

###########----------------------------------------------------------###########

#Visualization 2: Number of Hazards, Shocks or Stresses


#Visualization 3: Children population
#Visualization 4: The Children’s Climate Risk Index (CCRI)
#Visualization 5: Heatwave exposure each year
#Visualization 6: Temperature anamoly
#Visualization 7: Water scarcity
#Visualization 8: Season variablity
#Visualization 9: Drought frequency
#Visualization 10: Ground water decline
