library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggstream)
library(maps)
library(plotly)
library(reshape2)
library(wesanderson)

# setwd("C:/Users/liama/Downloads/INFO 201 Workspace/a3-Coevetive/source")
setwd("D:/Downloads/INFO Workspace/a3-Coevetive/source")

read_incarceration_trends <- function(){
  # USE FOR ONLINE VERSION:
  # filepath <- "https://raw.githubusercontent.com/vera-institute/incarceration  -trends/master/incarceration_trends.csv"
  filepath <- "incarceration_trends.csv"
  df <- read.csv(filepath)
  return(df)
}

incarceration_trends <- read_incarceration_trends()

# --- SUMMARY VALUES ---

# 877,113 less prisoners from 2017 to 2018 nationally!
avg_change_in_white_prision_pop_2017_2018 <- incarceration_trends %>%
  # filter for 2017 & 2018
  filter(year == 2018 | year == 2017) %>%
  # grab only needed columns
  select("year", "white_pop_15to64") %>%
  # group by year
  group_by(year)%>%
  # sum population values for 2018 and 2017
  summarize(total_us_white_pop = sum(white_pop_15to64)) %>% 
  # calculate diff between 2018 and 2017
  mutate(diff = total_us_white_pop - lag(total_us_white_pop))%>%
  # get rid of NA value generated from above
  na.omit()%>%
  # pull diff
  pull(diff)

# 173,324 more prisoners from 2017 to 2018 nationally!
avg_change_in_black_prision_pop_2017_2018 <- incarceration_trends %>%
  # filter for 2017 & 2018
  filter(year == 2018 | year == 2017) %>%
  # grab only needed columns
  select("year", "black_pop_15to64") %>%
  # group by year
  group_by(year)%>%
  # sum population values for 2018 and 2017
  summarize(total_us_black_pop = sum(black_pop_15to64)) %>% 
  # calculate diff between 2018 and 2017
  mutate(diff = total_us_black_pop - lag(total_us_black_pop))%>%
  # get rid of NA value generated from above
  na.omit()%>%
  # pull diff
  pull(diff)

# The highest recorded black population was in 2018.
highest_black_pop_year <- incarceration_trends %>% 
  # group by year
  group_by(year) %>% 
  # sum black pop for each year
  summarize(total_us_black_pop = sum(black_pop_15to64)) %>% 
  # omit NA values from 1970-1989
  na.omit() %>%
  # filter for highest pop value
  filter(total_us_black_pop == max(total_us_black_pop)) %>%
  # pull year
  pull(year)

# The lowest recorded black population was 28,834,577!
highest_black_pop_num <- incarceration_trends %>% 
  # group by year
  group_by(year) %>% 
  # sum black pop for each year
  summarize(total_us_black_pop = sum(black_pop_15to64)) %>% 
  # omit NA values from 1970-1989
  na.omit() %>%
  # filter for highest pop value
  filter(total_us_black_pop == max(total_us_black_pop)) %>%
  # pull pop
  pull(total_us_black_pop)

# The lowest recorded black population was in 1990.
lowest_black_pop_year <- incarceration_trends %>% 
  # group by year
  group_by(year) %>% 
  # sum black pop for each year
  summarize(total_us_black_pop = sum(black_pop_15to64)) %>% 
  # omit NA values from 1970-1989
  na.omit() %>%
  # filter for lowest pop value
  filter(total_us_black_pop == min(total_us_black_pop)) %>%
  # pull year
  pull(year)

# The lowest recorded black population was 19,000,362!
lowest_black_pop_num <- incarceration_trends %>% 
  # group by year
  group_by(year) %>% 
  # sum black pop for each year
  summarize(total_us_black_pop = sum(black_pop_15to64)) %>% 
  # omit NA values from 1970-1989
  na.omit() %>%
  # filter for lowest pop value
  filter(total_us_black_pop == min(total_us_black_pop)) %>% 
  # pull pop
  pull(total_us_black_pop)

# 9,834,215 black prisoners have been incarcerated since 1990.
high_low_black_pop_diff <- highest_black_pop_num - lowest_black_pop_num

# --- TRENDS OVER TIME CODE ---

prision_pop_by_race <- incarceration_trends %>%
  group_by(year) %>% 
  summarize(total_pop = sum(total_pop_15to64),
            aapi_prop_15to64 = sum(aapi_pop_15to64) / sum(total_pop_15to64),
            black_prop_15to64 = sum(black_pop_15to64) / sum(total_pop_15to64),
            latinx_prop_15to64 = sum(latinx_pop_15to64) / sum(total_pop_15to64),
            native_prop_15to64 = sum(native_pop_15to64) / sum(total_pop_15to64),
            white_prop_15to64 = sum(white_pop_15to64) / sum(total_pop_15to64)
            ) %>%
  select("year",
         "aapi_prop_15to64",
         "black_prop_15to64",
         "latinx_prop_15to64",
         "native_prop_15to64",
         "white_prop_15to64"
          ) %>% 
  na.omit() %>% 
  melt(id.vars=c("year")) %>%
  rename(pop_type = variable, prop_num = value)

us_percent_pop_stream_graph <- ggplot(data = prision_pop_by_race, aes(x = year, y = prop_num, fill = pop_type)) +
  geom_stream(type = "proportional", color = 1, lwd = 0.25) +
  labs(
    title = "Population Percentage By Race in US",
    subtitle = "Measured in Percentage of Total Prison Population",
    x = "Years", 
    y = "% of Pop",
    fill = "Race:",
    caption = "Source: Vera Institute of Justice"
  ) +
  scale_fill_manual(values = wes_palette("BottleRocket2", n = 5),
                      labels = c("Asian American / 
Pacific Islander",
                                  "Black",
                                  "Latinx",
                                  "Native American",
                                  "White")) +
  theme_minimal()

# --- VARIABLE COMPARISION CODE ---

pop_by_race_and_county_type <- incarceration_trends %>% 
  filter(year == "2018") %>% 
  select(aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64, urbanicity) %>% 
  melt(id.vars=c("urbanicity"))

temp <- incarceration_trends %>% 
  filter(year == "2018" & state == "WA") %>% 
  select(black_pop_15to64, white_pop_15to64)

ggplot(data = temp, mapping = aes(x = black_pop_15to64, y = white_pop_15to64)) +
  geom_point()

# --- MAP CODE ---

# Clean data and merge map_data to dataset.

us_county_shapes <- map_data("county") %>%
  unite(col="polyname", c('region','subregion'), sep = ",") %>%
  left_join(county.fips, by = "polyname")
  
incarceration_map_data <- incarceration_trends %>%
  filter(year == 2018,
         state == "WA") %>%
  left_join(us_county_shapes, by = "fips")

# Create map plot.

incarceration_map <- ggplot(data = incarceration_map_data) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = log(black_pop_15to64, 10)), color = NA) +
  coord_map()+
  theme_void()
