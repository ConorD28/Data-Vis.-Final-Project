#Data Vis. Final Project
#Gas Graphs:
library(maps)
library(ggplot2)
library(dplyr)
library(png)
library(gifski)
library(gganimate)

#Graph1:
#https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html
#https://ourworldindata.org/grapher/per-capita-gas?tab=map
gas = read.csv("per-capita-gas.csv", header = TRUE)
head(gas)
names(gas) = c(names(gas)[1:3], "Gas_Consumption")
gas2 = gas %>% filter(Year == 2021)
head(gas2)

world_map = map_data("world")

gas2021 <- gas2 %>% filter(Entity != "World") %>%
  mutate(colorGroup = cut(Gas_Consumption,
                          breaks = c(0, 5000, 10000, 20000, 30000, 40000, 140000),
                          labels = c('0-5,000 kWh', '5,000-10,000 kWh', '10,000-20,000 kWh',
                                     '20,000-30,000 kWh', '30,000-40,000 kWh', '40,000+ kWh'),
                          include.lowest = T
  )
  )


for (i in 1:length(gas2021$Entity)) {
  if (!gas2021$Entity[i] %in% world_map$region) {
    print(gas2021$Entity[i])
  }
}

gas2021$Entity[gas2021$Entity == 'Czechia'] <- 'Czech Republic'
gas2021$Entity[gas2021$Entity == 'United States'] <- 'USA'
gas2021$Entity[gas2021$Entity == 'United Kingom'] <- 'UK'

gasMap <- left_join(world_map, gas2021, by = c('region' = 'Entity'))
gasMap = gasMap %>% filter(region != "Antarctica")

colors <- c('#edf8fb', '#bfd3e6', '#9ebcda', '#8c96c6', 
            '#8856a7', '#810f7c')

ggplot(gasMap, aes(long, lat, group = group, fill = colorGroup)) + geom_polygon(color = "black") + theme_void() +
  scale_fill_manual(values = colors, na.value = 'grey', drop = F, 
                    labels = (c('0-5,000 kWh', '5,000-10,000 kWh', '10,000-20,000 kWh',
                                '20,000-30,000 kWh', '30,000-40,000 kWh', '40,000+ kWh', 'No data'))) +
  theme(legend.position = "bottom") + labs(fill = "", 
                                           title = "                        Per Capita Gas Consumption, 2021")

#interpretation of Graph1.

#Graph2:
#https://ourworldindata.org/grapher/gas-share-energy?tab=chart
gas_primary = read.csv("gas-share-energy.csv", header = TRUE)
