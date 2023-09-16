#Load necessary packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("zoo")
install.packages("maps")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)
library(maps)

# Load data from (https://github.com/vera-institute/incarceration-trends/blob/master/incarceration_trends.csv)

prison_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# Variable 1: Out of all the counties, which has the highest incarcerated population in the current year by race

highest_black_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))
highest_black_jail_state <- highest_black_jail_curr %>%
  pull(state)
highest_black_jail_county <- highest_black_jail_curr %>%
  pull(county_name)
highest_black_jail_pop <- highest_black_jail_curr %>%
  pull(black_jail_pop)
  
highest_white_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE))
highest_white_jail_state <- highest_white_jail_curr %>%
  pull(state)
highest_white_jail_county <- highest_white_jail_curr %>%
  pull(county_name)
highest_white_jail_pop <- highest_white_jail_curr %>%
  pull(white_jail_pop)

highest_latinx_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE))
highest_latinx_jail_state <- highest_latinx_jail_curr %>%
  pull(state)
highest_latinx_jail_county <- highest_latinx_jail_curr %>%
  pull(county_name)
highest_latinx_jail_pop <- highest_latinx_jail_curr %>%
  pull(latinx_jail_pop)

highest_aapi_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE))
highest_aapi_jail_state <- highest_aapi_jail_curr %>%
  pull(state)
highest_aapi_jail_county <- highest_aapi_jail_curr %>%
  pull(county_name)
highest_aapi_jail_pop <- highest_aapi_jail_curr %>%
  pull(aapi_jail_pop)

highest_native_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE))
highest_native_jail_state <- highest_native_jail_curr %>%
  pull(state)
highest_native_jail_county <- highest_native_jail_curr %>%
  pull(county_name)
highest_native_jail_pop <- highest_native_jail_curr %>%
  pull(native_jail_pop)

highest_other_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE))
highest_other_jail_state <- highest_other_jail_curr %>%
  pull(state)
highest_other_jail_county <- highest_other_jail_curr %>%
  pull(county_name)
highest_other_jail_pop <- highest_other_jail_curr %>%
  pull(other_race_jail_pop)

# Variable 2: Out of all the counties, which has the highest incarcerated to ratio of total black vs white population?

inc_ratio_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%
  mutate(black_jail_ratio = as.numeric(black_jail_pop/black_pop_15to64)) %>%
  mutate(white_jail_ratio = as.numeric(white_jail_pop/white_pop_15to64))

highest_ratio_black <- inc_ratio_curr %>%
  filter(black_jail_ratio == max(black_jail_ratio))
highest_ratio_black_inc <- highest_ratio_black %>%
  pull(black_jail_ratio)
highest_ratio_black_county <- highest_ratio_black %>%
  pull(county_name)
highest_ratio_black_state <- highest_ratio_black %>%
  pull(state)

highest_ratio_white <- inc_ratio_curr %>%
  filter(white_jail_ratio == max(white_jail_ratio))
highest_ratio_white_inc <- highest_ratio_white %>%
  pull(white_jail_ratio)
highest_ratio_white_county <- highest_ratio_white %>%
  pull(county_name)
highest_ratio_white_state <- highest_ratio_white %>%
  pull(state)

# Variable 3: In the most recent year, what is the mean ratio of the incarcerated black vs white population 

mean_ratio_black_inc <- mean(inc_ratio_curr$black_jail_ratio, na.rm = TRUE)

mean_ratio_white_inc <- mean(inc_ratio_curr$white_jail_ratio, na.rm = TRUE)

# Variable 4: Ratio of incarcerated black population compared to white population in King County, WA in most recent year (2018)

curr_king <- prison_data %>%
  filter(year == max(year)) %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(state == "WA") %>%
  filter(county_name == "King County")

perc_black_king_total_curr <- (curr_king$black_pop_15to64/curr_king$total_pop_15to64) * 100
perc_white_king_total_curr <- (curr_king$white_pop_15to64/curr_king$total_pop_15to64) * 100

perc_black_king_jail_curr <- (curr_king$black_jail_pop / curr_king$total_jail_pop) * 100
perc_white_king_jail_curr <- (curr_king$white_jail_pop / curr_king$total_jail_pop) * 100

# Variable 5: Ratio of incarcerated black population compared to white population in King County, WA in 1990

past_king <- prison_data %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  filter(year == min(year))

perc_black_king_total_past <- (past_king$black_pop_15to64/past_king$total_pop_15to64) * 100
perc_white_king_total_past <- (past_king$white_pop_15to64/past_king$total_pop_15to64) * 100

perc_black_king_jail_past <- (past_king$black_jail_pop / past_king$total_jail_pop) * 100
perc_white_king_jail_past <- (past_king$white_jail_pop / past_king$total_jail_pop) * 100

# Chart 1: Number of incarcerated black people vs incarcerated white people in King County over time

king_inc_ratio_year <- prison_data %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64))

white_black_ratio_king_year <- ggplot(data = king_inc_ratio_year) +
  geom_line(aes(x = year, y = black_jail_pop, color = "Black"), show.legend = TRUE) +
  geom_line(aes(x = year, y = white_jail_pop, color = "White"), show.legend = TRUE) +
  scale_color_manual(name = "Race",
                     values = c("White" = "red", "Black" = "blue")) +
  labs(x = "Year", y = "Number of incarcerated individuals (hundreds)") +
  theme(legend.position = "right") +
  ggtitle("Number of White and Black incarcerated peoples in King County from 1990 to 2018")

print(white_black_ratio_king_year)

# Chart 2: Ratio of black to white population vs ratio of black to white inc population in US

king_curr_inc_race <- inc_ratio_curr %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  filter(year == max(year)) %>%
  mutate(latinx_jail_ratio = as.numeric(latinx_jail_pop/latinx_pop_15to64)) %>%
  mutate(aapi_jail_ratio = as.numeric(aapi_jail_pop/aapi_pop_15to64)) %>%
  mutate(native_jail_ratio = as.numeric(native_jail_pop/native_pop_15to64))


king_race_data <- data.frame(
  race = c("Black", "White", "Latinx", "AAPI", "Native"),
  inc_ratio = c(king_curr_inc_race$black_jail_ratio,
                king_curr_inc_race$white_jail_ratio,
                king_curr_inc_race$latinx_jail_ratio,
                king_curr_inc_race$aapi_jail_ratio,
                king_curr_inc_race$native_jail_ratio)
)

race_king_chart <- ggplot(king_race_data, aes(x = race, y = inc_ratio)) +
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Ratio") +
  ggtitle("Ratio of incarcerated individuals to total racial population by race in King County in 2018")

print(race_king_chart)


# Chart 3: Map of ratio of African American incarceration

bt <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank()
  )

us_regions <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- us_regions %>%
  left_join(inc_ratio_curr, by = "fips")

black_inc_map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_jail_ratio),
               color = "white",
               size = 0.0001) +
  coord_map() +
  scale_fill_continuous(na.value = "gray", low = "blue", high = "red") +
  ggtitle("Map of ratio of incarcerated African Americans to total African American population by county in 2018") +
  bt

print(black_inc_map)






