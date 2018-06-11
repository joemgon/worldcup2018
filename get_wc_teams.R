# load libraries
library(tidyverse) # for data manipulation
library(rvest) # for web scraping
library(lubridate) # to work with the birthdates
library(tictoc) # for timing the script
library(DT) # for interactive tables

# start timer
tic()

# url for the 2018 WC squads

url <- "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads"

# iterate through all 32 tables

# two teams are still missing:

# Serbia: The final squad will be announced on 24 May 2018. -- xpath: 19

# Switzerland: The final squad will be announced on 4 June 2018.  -- xpath: 20

# make a vector for all table numbers (will be 32 but for now 30)
table_numbers <- c(1:32)

# function to get table for each team
get_teams <- function(tbl_nums) {
  #headers <- map_int(tbl_nums, ~if_else(.>19,as.integer(.+1),.))  # skip 19 and 20 for now
  
  # get country names
  team_name <-   url %>%
    read_html() %>%
    html_node(xpath = paste0('//*[@id="mw-content-text"]/div/h3[',tbl_nums,']')) %>%
    html_text
  
  # get squad info
  url %>%
    read_html() %>%
    html_node(xpath = paste0('//*[@id="mw-content-text"]/div/table[',tbl_nums,']')) %>%
    html_table(fill = TRUE) %>%
    as.tibble() %>%
    mutate(team_name = team_name) # add country names
}

# get squad infor for all teams
teams <- map_df(table_numbers, get_teams)

# clean up the data
teams <- teams %>%
  #filter(!is.na(Caps)) %>%
  # select(-`0#0`) %>%
  # add and change columns to match historical data
  mutate(`No.` = as.character(`No.`), ClubCountry = NA, Year = 2018) %>%
  rename(No = `No.`, Pos = `Pos.`, `DOB/Age` = `Date of birth (age)`, Country = team_name) %>%
  # select(-Goals) %>%
  select(No,Pos,Player,`DOB/Age`,Caps,Club,Country,ClubCountry,Year)

## read in historical data
historic_data <- read_csv("https://raw.githubusercontent.com/sanand0/fifadata/master/squads.csv")

# change data type for Caps to match
historic_data <- historic_data %>%
  mutate(Caps = as.integer(Caps))

# stack 2018 data and historical data
all_squads <- bind_rows(historic_data,teams)

# stop timer
toc()

# runs in 45.272 sec


# feature creation

# ## one rwo per year to look for inconsistencies
# one_row_per_year <- all_squads %>% 
#   group_by(Year) %>%
#   filter(row_number() == 1) %>%
#   ungroup()

all_squads <- all_squads %>%
  mutate(age = as.integer(str_extract(`DOB/Age`, "(?<=aged ?)\\d+")),
         month = str_extract(`DOB/Age`, "[:alpha:]+"),
         dob = as.Date(str_extract(`DOB/Age`, "[[:digit:],-]+")),
         birth_year = lubridate::year(dob),
         birth_month = lubridate::month(dob),
         birth_day = lubridate::day(dob))

## write the file
write_csv(all_squads,"squads.csv")

## make a data table

int_table <- all_squads %>%
  filter(Year == 2018) %>%
  select(Pos,Player,`DOB/Age`,Caps,Country,Club) %>%
  mutate_at(c("Pos","Country"), ~as.factor(.))
  
datatable(int_table, filter = 'top', options = list(pageLength = 23))


# eda  (compare age, caps, clubs)

#!!# for all plots decide on a theme #!!#

# average age
all_squads %>%
  group_by(Year) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = avg_age)) +
  geom_point()

# median age
all_squads %>%
  group_by(Year) %>%
  summarize(avg_age = mean(age, na.rm = TRUE), median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = median_age)) +
  geom_point()

# average age by position by country for 2018
#!!# change order of countries #!!#
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country, Pos) %>%
  summarize(avg_age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(x = Country, y = avg_age)) +
  geom_point() +
  coord_flip() +
  facet_grid(~Pos)

# count clubs
#!!# make y axis whole numbers only #!!#
#!!# arrange by count -- reorder x axis #!!#
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Club) %>%
  tally() %>%
  arrange(n) %>%
  top_n(11) %>%
  ggplot(aes(x = factor(Club,levels=Club,ordered=TRUE), y = n)) +
  geom_point() +
  scale_y_continuous(breaks=seq(8,17,1)) +
  coord_flip() +
  labs(x = "Clubs", y = "Players in WC Squads", title = "WC Player Count by Club")

# average caps (over all time) 
all_squads %>%
  group_by(Year) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = avg_caps)) +
  geom_point()

# median caps (over all time)   ## plot for Twitter ##
all_squads %>%
  group_by(Year) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = median_caps)) +
  geom_point()

## avg caps and median caps 

# average cap
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  arrange(avg_caps) %>%
  ggplot(aes(x = factor(Country, levels=Country, ordered = TRUE), y = avg_caps)) +
  geom_point() +
  coord_flip()

# median cap
all_squads %>%
  filter(Year == 2018) %>%
  group_by(Country) %>%
  summarize(avg_caps = mean(Caps, na.rm = TRUE), median_caps = median(Caps, na.rm = TRUE)) %>%
  arrange(median_caps) %>%
  ggplot(aes(x = factor(Country, levels=Country, ordered = TRUE), y = median_caps)) +
  geom_point() +
  coord_flip()
  
## gladwell test (birth month)
#!!# switch to abbreevs #!!#
all_squads %>%
  group_by(month, birth_month) %>%
  filter(!is.na(month), !is.na(birth_month)) %>%
  tally() %>%
  arrange(birth_month) %>%
  ggplot(aes(x = factor(month, levels = month, ordered = TRUE), y = n)) +
  geom_point()
