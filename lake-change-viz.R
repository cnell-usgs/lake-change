# Visualizing Lake Change -------------------------------------------------
## Cee Nell cnell@usgs.gov twitteR: @cee_viz
## For GLEON GSA
## Thurs Oct 6, 2022

install.packages(c('tidyverse','lubridate','data.table','sf','spData','scales',
                   'ggdark','scico','gganimate'))

library(tidyverse)
library(lubridate) # dates
library(data.table)
library(sf) # vector spatial
library(spData) # map polygons
library(scales) # better scales
library(ggdark) # dark themes
library(scico) # color scales

# Data: GLCP -------------------------------------------------------------

## The Global Lake area, Climate, and Population (GLCP) dataset
## (https://portal.edirepository.org/nis/mapbrowse?packageid=edi.394.4) contains
## lake surface area data for over 1.42 million lakes globally from 1995-2015,
## in terms of seasonal, permanent, and total surface water.

## This script builds off of data processing steps 1 & 2 documented in
## https://github.com/USGS-VIZLAB/chart-challenge-22/blob/main/14_3D_mfmeyer/three_dimensional.R
## Created by Michael Meyer (mfmeyer@usgs.gov)
## Which could be applied to any global region in the GLCP
## Here we focus on the contiguous US (CONUS)

## GLCP filtered to CONUS
glcp <- fread('https://labs.waterdata.usgs.gov/visualizations/data/glcp_conus.csv')
str(glcp)
### permanent_km2 = water present for all observable months within a year
### seasonal_km2 = water present at least one month of all observable months within a year
### total_km2 = permanent_km2 + seasonal_km2

# Add spatial context -----------------------------------------------------

proj <- "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

states <- st_transform(spData::us_states, st_crs(proj)) # CONUS at state-level

glcp_sf <- glcp |>
  st_as_sf(coords = c('centr_lon', 'centr_lat'), crs = 4269) |> # make spatial
  st_transform(proj) |>
  st_intersection(states)
str(glcp_sf)

unique(glcp_sf$REGION)

## Filtering to a region to iterate more quickly
loc_filter <- 'Midwest'
glcp_region <- glcp_sf |>
  filter(REGION == loc_filter)
glcp_region

length(unique(glcp_region$Hylak_id)) # number of lakes

# Plot all the data -------------------------------------------------------

glcp_region |>
  ggplot(
    aes(x = year,
        y = total_km2,
        group = Hylak_id, # each line = 1 lake
        color = NAME
        ),
    alpha = 0.15
    ) +
  geom_line()
# yikes!


# Z-score lake surface areas ----------------------------------------------

## Use z-score to scale lake surface area variables
### how lakes change in area relative to the 20 year mean
glcp_scaled <- glcp_region |>
  group_by(Hylak_id) |>
  mutate(across(contains("_km2"), ~as.vector(scale(.x)), .names = "{.col}_scale"))
str(glcp_scaled)

glcp_scaled |>
  ggplot(
    aes(x = year,
        y = seasonal_km2_scale,
        group = Hylak_id,
        color = NAME
    ),
    alpha = 0.01
    ) +
  geom_line() +
  facet_wrap(~NAME)

## Use facets to show states through time
some_state <- 'Wisconsin' # any state in the region

glcp_scaled |>
  filter(NAME %in% some_state) |>
  ggplot() +
  geom_sf(data = states |> filter(NAME %in% some_state), fill = NA) +
  geom_sf(
    aes(color = total_km2_scale),
    size = 0.2
    ) +
  theme_void() +
  # scico color palettes: https://github.com/thomasp85/scico
  scale_color_scico(palette = "broc",
                    direction = -1,
                    # custom legend labels
                    breaks = scales::extended_breaks(5),
                    # 0 represents mean lake area over the time period
                    labels = c('smaller','', '', '', 'bigger')
                    ) +
  facet_wrap(~year, nrow = 3,
             strip.position = "bottom" # move year labels
             )# +
  #dark_theme_void() +
  #theme(legend.position = "top",
  #      plot.margin = margin(2, 2, 2, 2, 'lines'),
  #      strip.text = element_text(size = 16)) +
  ## custom legend styling https://ggplot2.tidyverse.org/reference/guides.html
  #guides(
  #  color = guide_colorbar(
  #    direction = "horizontal",
  #    barwidth = 16, barheight = 1,
  #    title = sprintf("Changing lake surface areas in %s", some_state),
  #    title.theme = element_text(face = "bold", size = 20, vjust = 1, color = "white"),
  #    label.theme = element_text(size = 12, color = "white"))
  #)

ggsave(sprintf("lake_sa_%s.png", some_state), width = 16, height = 10)

## Check out Michael's GLCP viz: https://twitter.com/USGS_DataSci/status/1515091100763049992

# Revert to geom default from ggdark
invert_geom_defaults()

# Data: U.S. Lake Temperatures --------------------------------------------------

## Daily surface temperature predictions for 185,549 U.S. lakes with associated observations and meteorological conditions (1980-2020)
## Willard, J., Read, J.S., Topp, S.N., Hansen, G.J.A., and Kumar, V., 2022, Daily surface temperature predictions for 185,549 U.S. lakes with associated observations and meteorological conditions (1980-2020): U.S. Geological Survey data release, https://doi.org/10.5066/P9CEMS0M.

## This script build off processing steps in https://github.com/USGS-VIZLAB/lake-temp-timeseries for daily data across the full time period, and a single lake (nhdhr_143249470)

## Daily surface temperature predictions for Lake Mendota, WI
mendota <- fread('https://labs.waterdata.usgs.gov/visualizations/data/surftemp_mendota.csv') |>
  transform(date = as.Date(date)) |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE),
         yday = lubridate::yday(date)) # day of the year
str(mendota)

## Plot it all
mendota |>
  ggplot(aes(date, surftemp)) +
  geom_point(size = 2, aes(color = surftemp)) +
  scale_color_scico(palette = "roma", direction = -1) +
  scale_x_date(breaks = scales::breaks_width('2 years'),
               labels = scales::label_date_short()) +
  labs(x = NULL, y = "Surface Temperature (C)") +
  theme(legend.position = 'none')

## Find mean daily temps from 1981 to 1990
mendota_past <- mendota |>
  filter(year < 1991) |>
  group_by(yday) |>
  summarize(hist_mean = mean(surftemp))
mendota_past

## Join with data from 1991 - 2020 and calculate difference in degrees
mendota_diff <- mendota |>
  filter(year >= 1991) |>
  left_join(mendota_past) |>
  mutate(surftemp_diff = surftemp - hist_mean)

## Plot the difference in surftemps
mendota_diff |>
  ggplot(aes(date, surftemp_diff)) +
  geom_point(size = 2, aes(color = surftemp_diff)) +
  scale_color_scico(palette = "roma", direction = -1, midpoint = 0) +
  scale_x_date(breaks = scales::breaks_width('2 years'),
               labels = scales::label_date_short()) +
  labs(x = NULL, y = "Temperature difference from 10 year mean (1980 - 1991)") +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = "dotted")

# adding fonts ------------------------------------------------------------

library(showtext)

# google fonts - https://fonts.google.com/
font_fam <- 'Source Sans Pro'
font_add_google(font_fam, regular.wt = 300, bold.wt = 700)


## Clean it up
mendota_diff |>
  ggplot(aes(date, surftemp_diff)) +
  geom_point(size = 2, aes(color = surftemp_diff)) +
  #geom_bar(stat = 'identity', aes(fill = surftemp_diff), color = NA) +
  scale_color_scico(palette = "roma", direction = -1, midpoint = 0) +
  scale_x_date(breaks = scales::breaks_width('2 years'),
               labels = scales::label_date_short(),
               position = "top") +
  labs(x = NULL, y = NULL) +
  ggtitle("Lake Mendota surface temperatures (C)",
          subtitle = "difference from 10-year mean (1980-1990)") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 16) +
  theme(text = element_text(family = font_fam),
        title = element_text(size = 24, face = "bold"),
        legend.position = 'none',
        plot.background = element_rect(fill = "white", color = NA))

ggsave('mendota_surftemp_diff.png', width = 16, height = 9)

# Coord_polar -------------------------------------------------------------

library(gganimate)

month_labels <- mendota_diff |>
  group_by(month) |>
  summarize(month_start = min(yday))

anim <- mendota_diff |>
  ggplot(aes(yday, surftemp_diff)) +
  geom_point(size = 2, aes(color = surftemp_diff, group = year)) +
  scale_color_scico(palette = "vik", direction = 1,
                    midpoint = 0
                    ) +
  # create a circular chart
  coord_polar() +
  # add line representing no difference
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(breaks = month_labels$month_start,
                     labels = month_labels$month) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(axis.text.y = element_blank(),
        legend.position = 'bottom') +
  # animate in time
  transition_time(year) + # try transition_reveal
  shadow_mark(alpha = 0.75) +
  ggtitle("{as.integer(frame_time)}") +
  # style legend
  guides(
    color = guide_colorbar(
      direction = "horizontal",
      barwidth = 16, barheight = 1,
      title = 'Temperature difference (C)\nfrom mean (1980-1990)')
  )


animate(
  anim,
  nframes = length(unique(mendota_diff$year)),
  fps = 2,
  width = 500, height = 600
)

anim_save('lake_temp_polar.gif')
