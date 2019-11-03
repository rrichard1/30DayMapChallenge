################################################################################
## Project: Richard_NCAA_BB_Map
## Script purpose: Replicate FiveThirtyEight NCAA College Basketball Map
## Date: 26-Sep-2019
## Author:Roland Richard
################################################################################
library(tidyverse)
library(rio)
library(sf)
library(lwgeom)
library(tmap)
library(tmaptools)
library(spData)
library(tigris)
library(grid)
library(extrafont)
library(ggrepel)
options(tigris_use_cache = TRUE)

# load Data prepared in "code/Richard_NCAA_BB_Data_Query_Prep.R"
conf_loc <- import("conf_tourn_locations.csv")
ncaa_mbb <- import("ncaa_mbb_all_d1.rds")
power6 <- import("ncaa_mbb_power6.rds")
power6Geo <- import("ncaa_mbb_power6_geocoded.rds")

# Function to create lines using great circles
journeys_to_sf <- function(journeys_data,
                           start_long = start.long,
                           start_lat = start.lat,
                           end_long = end.long,
                           end_lat = end.lat) {
  quo_start_long <- enquo(start_long)
  quo_start_lat <- enquo(start_lat)
  quo_end_long <- enquo(end_long)
  quo_end_lat <- enquo(end_lat)

  journeys_data %>%
    select(
      !! quo_start_long,
      !! quo_start_lat,
      !! quo_end_long,
      !! quo_end_lat
    ) %>%
    transpose() %>%
    map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>%
    map(st_linestring) %>%
    st_sfc(crs = 4326) %>%
    st_sf(geometry = .) %>%
    bind_cols(journeys_data) %>%
    select(everything(), geometry)
}

# Load US State Map -------------------------------------------------------

# US State abbreviations
us <- unique(fips_codes$state)[c(1:51)]
cont <- us[-c(2,12)]

# US State Polygon file
states <- states(cb = TRUE)

# Extract Lower 48 States (No Power 6 Schools in AK or HI)
l48 <- st_as_sf(states) %>%
  filter(STUSPS %in% cont) %>%
  st_transform(crs = 2163)


# Connect school points by conference

conf_schls <- power6Geo %>%
  filter(type == "school") %>%
  select(conf_nm, alias,lon,lat) %>%
  st_drop_geometry() %>%
  set_names(c("conf_nm", "alias", "start.long", "start.lat"))


conf_ctrs <- power6Geo %>%
  filter(type == "center") %>%
  select(conf_nm,lon,lat) %>%
  st_drop_geometry() %>%
  set_names(c("conf_nm", "end.long", "end.lat"))

conf_hubs <-
  left_join(conf_schls, conf_ctrs) %>%
  journeys_to_sf() %>%
  st_segmentize(units::set_units(100, km))


# Create Conference footprint boundary
conf_poly <- power6Geo %>%
  filter(type != "tourney") %>%
  group_by(conf_nm) %>%
  summarise() %>%
  st_cast("POLYGON") %>%
  st_convex_hull() %>%
  st_transform(crs = 2163)

# convert to buffer to surround points
conf_bounds <- conf_poly %>% st_buffer(dist = 35000,endCapStyle = "SQUARE")

# merge with conference location data for labels
tourn_labels <- power6Geo %>% filter(type == 'tourney') %>% left_join(conf_loc)
x_range <- abs(Reduce("-", range(tourn_labels$lon)))
y_range <- abs(Reduce("-", range(tourn_labels$lat)))
tourn_labels$nudge_x <- 0.6 * x_range
tourn_labels$nudge_y <- 0.9 * y_range

tourn_labels <- tourn_labels %>%
  mutate(nudge_x = case_when(conf_alias == "BIG12" ~ -20.1,
                             conf_alias %in% c("ACC", "BIG10", "BIGEAST")~ 50,
                             TRUE ~ as.double(nudge_x) ))

# Map Power 6 Programs and Conf. Tournament Locations ---------------------

power6_map <- ggplot() +
  geom_sf(
    data = l48,
    fill = '#303030',
    color = '#E8E8E8',
    lwd = 0.1
  ) +
  geom_sf(
    data = conf_hubs,
    aes(group = conf_nm),
    color = '#FF00FF',
    lwd = 1,
    show.legend = FALSE
  ) +
  geom_sf(
    data = power6Geo,
    aes(
      fill = type,
      color = type,
      size = type
    ),
    shape = 21,
    alpha = 0.9,
    show.legend = "point"
  ) +
  geom_sf(
    data = conf_bounds,
    aes(group = conf_nm),
    fill = '#FF00CC',
    color = '#FFFFFF',
    alpha = 0.20
  ) +
  geom_sf(
    data = tourn_labels,
    fill = "#3300FF",
    color = "#FFFFFF",
    size = 5,
    shape = 21,
    show.legend = "point"
  ) +
  geom_text_repel(
    data  =
      tourn_labels %>% filter(conf_alias %in% c("ACC", "BIG10", "BIGEAST")),
    aes(x = lon, y = lat, label = loc_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.9,
    nudge_x = tourn_labels$nudge_x,
    nudge_y = tourn_labels$nudge_y,
    direction    = "y",
    col = "#000000",
    fontface = "bold"
  ) +
  geom_text_repel(
    data  =
      tourn_labels %>% filter(conf_alias %in% c("BIG12", "PAC12", "SEC")),
    aes(x = lon, y = lat, label = loc_label, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.9,
    nudge_x = tourn_labels$nudge_x,
    nudge_y = tourn_labels$nudge_y,
    direction    = "x",
    col = "#FFFFFF",
    fontface = "bold"
  ) +
  scale_fill_manual(
    name = "Location:",
    values =  c("#FF00CC","#FF00CC","#3300FF"),
    labels = c(
      "School",
      "Conference Geographic Center",
      "Conference Tournament Location"
    )
  ) +
  scale_color_manual(
    name = "Location:",
    values =  c("#FF00FF","#FFFFFF","#FFFFFF"),
    labels = c(
      "School",
      "Conference Geographic Center",
      "Conference Tournament Location"
    )
  ) +
  scale_size_manual(
    name = "Location:",
    values = c(2, 6, 5),
    labels = c(
      "School",
      "Conference Geographic Center",
      "Conference Tournament Location"
    )
  ) +
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(
           shape = 21,
           size = 5,
           fill = c("#FF00CC", "#FF00CC", "#3300FF"),
           color = c("#FF00FF","#FFFFFF","#FFFFFF")
         ))) +
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2500000, 2500000),
    ylim = c(-2300000, 730000),
    clip = "on"
  ) +
  facet_wrap(.~conf_nm,nrow = 2) +
  labs(title = "Conference championships are far from the conference centers",
       subtitle = "Geographic centers of each of the top six men's college basketball conferences and the\nlocations of each of their 2018 conference tournaments\n",
       caption = " Originally Created by FiveThirtyEight\n Recreated by Roland Richard (@rorich)\n") +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    legend.position = c(0.5, 0),
    legend.justification = c(0, 0),
    legend.direction = "horizontal",
    legend.box.just = "center",
    legend.title = element_text(size = 12, face = "bold", hjust = 1),
    legend.text = element_text(size = 10, hjust = 0.5),
    strip.background = element_blank(),
    strip.text.x = element_text(face = 'bold',  size = 16, hjust = 0.5, vjust = 0.5,margin = margin(0.5,0,0.5,0, "cm")),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "#454545", size = 18, face = "plain", hjust = 0.5),
    plot.caption = element_text(size = 10, face = "plain", color = "#FFFFFF", hjust = 0, vjust = 1),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#999999")
  )

power6_map + ggsave(filename = 'ncaa_conf_map.png', device = 'png' ,width = 17, height = 11, units = "in",dpi = 300)

