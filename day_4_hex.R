# Louisiana Racial/Ethnic Diversity by Parish

### Load Packages --------------------------------------------------------------
library(tidyverse) # For data wrangling; also loads ggplot2
library(sf) # For reading, writing and working with spatial objects (Vector data)
library(tmap)# For creating maps
library(tmaptools)# For reading and processing spatial data related to tmap
library(leaflet) # for interactive mapping
library(mapview)# another option for interactive mapping
library(tidycensus) # For downloading Census data
library(spData) # Loads geographic data
library(tigris) # Socio-demographic vector data for the USA
library(rio)
library(OasisR)
library(geogrid)
library(viridis)

options(tigris_use_cache = TRUE)

# Racial/Ethnic Composition----------------------------------------------------

# ACS Race Variables

race_vars <- c(
  tot_pop = "B03002_001",
  non_hisp = "B03002_002",
  white = "B03002_003",
  black = "B03002_004",
  naan = "B03002_005",
  asian = "B03002_006",
  nhpi = "B03002_007",
  other = "B03002_008",
  multi = "B03002_009",
  hispanic = "B03002_012"
)

# Data Extract

race  <- get_acs(
  geography = "county",
  state = 22,
  variables = race_vars ,
  year = 2017,
  output = "tidy",
  cache_table = TRUE
) %>%
  select(-moe) %>%
  spread(variable, estimate)

dfRace <- race %>%
  mutate(other = multi + naan + nhpi + other) %>%
  select(GEOID, NAME, tot_pop, white, black, hispanic, asian, other) %>%
  gather(race_group, race_pop, -tot_pop, -NAME, -GEOID) %>%
  group_by(GEOID, race_group) %>%
  mutate(pct_race = race_pop / tot_pop) %>%
  ungroup %>%
  select(GEOID, NAME, race_group, pct_race) %>%
  spread(race_group, pct_race) %>%
  replace(is.na(.), 0)


# Calculate Racial/Ethnic Diversity ---------------------------------------

race_n <- race %>%
  select(GEOID,everything(),-tot_pop, -non_hisp, -NAME) %>%
  column_to_rownames(var = "GEOID") %>% as.matrix

la_parishes <- counties(state= "LA", cb = TRUE,resolution = '5m')

la_parishes <- st_as_sf(la_parishes)

x <- segdataclean(x = race_n)

dfSeg1 <- enframe(LShannon(x[["x"]]),name = "GEOID", value = "LShan")
dfSeg2 <- enframe(HLoc(x[["x"]]),name = "GEOID", value = "NShan")
dfSeg3 <- enframe(LSimpson(x[["x"]]),name = "GEOID", value = "LSimp")

dfSeg <- dfSeg1 %>% left_join(dfSeg2) %>% left_join(dfSeg3)
dfRaceSeg <- left_join(dfRace,dfSeg)

dfDiversity <- dfSeg1 %>% set_names(c("GEOID", "Diversity"))

dfMap <- left_join(la_parishes, dfDiversity)


# Generate Hexagons -------------------------------------------------------

original_shapes <- dfMap
original_shapes$SNAME <- substr(original_shapes$NAME, 1, 4)


par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}


new_cells_hex <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = 5)
resulthex <- assign_polygons(original_shapes, new_cells_hex)

theme_map <- function(...) {
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 18, hjust = 0, color =  '#303030', face = 'bold'),
    plot.subtitle = element_text(size = 14, hjust = 0, color = '#303030', face = 'bold'),
    plot.caption = element_text(size = 9, hjust = 0, color =  '#303030'),
    # remove all axes
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    # panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 12, hjust = 0, color = "#303030"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", colour = "#303030", size = 14)
  )
}


hexplot <-
  tm_shape(resulthex) +
  tm_polygons("Diversity", palette = "viridis", ) +
  tm_text("NAME", size = 0.6)
hexplot

resulthex <- resulthex %>% mutate(div_breaks = ntile(desc(Diversity),n = 7))

tlr <- c('#009392','#72aaa1','#b1c7b3','#f1eac8','#e5b9ad','#d98994','#d0587e')


ggplot() +
  geom_sf(data = resulthex,
          aes(fill = factor(div_breaks)),
          color = '#E8E8E8',
          lwd = 0.1) +
  geom_text(data = resulthex,
            aes(
              x = V1,
              y = V2,
              label = stringr::str_wrap(NAME, width = 5)
            ),
            size = 3.5,) +
  scale_fill_manual(
    "Parish Diversity:",
    values = tlr ,
    labels = c("High", "", "", "Moderate", "", "", "Low")
  ) +
  labs(title = " Louisiana Racial/Ethnic Diversity by Parish",
       subtitle = " #30DayMapChallenge: Day 4",
       caption = " Data Source:US Census ACS 2013-17 5-Year Estimates\n Created By: Roland Richard (@rorich)\n") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    # legend.position = c(0.5, 0),
    # legend.justification = c(0, 0),
    # legend.direction = "horizontal",
    # legend.box.just = "center",
    legend.title = element_text(size = 12, face = "bold", hjust = 1),
    legend.text = element_text(size = 10, hjust = 0.5),
    strip.background = element_blank(),
    strip.text.x = element_text(
      face = 'bold',
      size = 16,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(0.5, 0, 0.5, 0, "cm")
    ),
    plot.title = element_text(size = 24, face = "bold", hjust = 0),
    plot.subtitle = element_text(
      color = "#454545",
      size = 18,
      face = "plain",
      hjust = 0
    ),
    plot.caption = element_text(size = 9, hjust = 0),
    panel.background = element_blank()
  ) +
  ggsave(
    filename = 'la_diversity_hex.png',
    device = 'png' ,
    width = 10,
    height = 11,
    units = "in",
    dpi = 300
  )
